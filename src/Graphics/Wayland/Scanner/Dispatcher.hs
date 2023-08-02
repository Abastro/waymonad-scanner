{-# LANGUAGE TemplateHaskellQuotes #-}

module Graphics.Wayland.Scanner.Dispatcher (
  makeDispatcher,
)
where

import Control.Monad
import Data.IORef
import Data.Word (Word32)
import Foreign.Ptr (FunPtr, Ptr, freeHaskellFunPtr, nullPtr)
import Foreign.StablePtr (castPtrToStablePtr, castStablePtrToPtr, deRefStablePtr, freeStablePtr, newStablePtr)

import Graphics.Wayland.Scanner.Marshal
import Graphics.Wayland.Scanner.Types
import Graphics.Wayland.Scanner.WLS
import Graphics.Wayland.Server.Resource (Resource)
import Graphics.Wayland.Util.Types (Argument, Message)
import Utility

import Language.Haskell.TH qualified as TH

makeDispatchClause :: (TH.Name, [TH.Name]) -> [[ArgumentType]] -> Scan TH.Clause
makeDispatchClause (dataTName, fs) xs = do
  marshalDecs <- undefined <$> zipWithM makeMarshaller marshalNames xs
  TH.clause [TH.varP implName, TH.wildP, TH.varP opName, TH.wildP, TH.varP argName] (TH.normalB funExp) (concat @[] marshalDecs)
 where
  implName = TH.mkName "implPtr"
  opName = TH.mkName "opcode"
  argName = TH.mkName "wlArgs"
  dataName = TH.mkName "takers"

  makeMarshalName idx = TH.mkName ("marshal" ++ show idx)
  marshalNames = take (length xs) $ fmap makeMarshalName [0 :: Int ..]

  dataStmt = TH.bindS (TH.sigP (TH.varP dataName) (TH.conT dataTName)) [e|deRefStablePtr (castPtrToStablePtr $(TH.varE implName))|]
  makeMatch field index =
    let matchExp = TH.appE (TH.appE [e|$(TH.varE $ makeMarshalName index)|] (TH.varE argName)) (TH.appE (TH.varE field) (TH.varE dataName))
     in TH.match (TH.litP (TH.IntegerL index)) (TH.normalB matchExp) []
  failClause = TH.match TH.wildP (TH.normalB $ TH.appE (TH.varE 'pure) (TH.conE '())) []
  clauses = zipWith makeMatch fs [0 ..] ++ [failClause]
  funExp = TH.doE [dataStmt, TH.noBindS (TH.caseE (TH.varE opName) clauses)]

makeDispatchRecord :: String -> [(String, [ArgumentType])] -> Scan TH.Dec
makeDispatchRecord name xs = do
  let dataName = TH.mkName $ cleanName name ++ "Requests"
      bang = TH.Bang TH.NoSourceUnpackedness TH.SourceStrict
      makeField (field, args) = do
        tType <- undefined <$> takerType args
        pure (TH.mkName (replaceUnder name ++ "Request" ++ cleanName field), bang, tType)
  dataCon <- TH.RecC dataName <$> mapM makeField xs
  pure $ TH.DataD [] dataName [] Nothing [dataCon] []

dispatcherType :: Scan TH.Type
dispatcherType = [t|Ptr () -> Ptr Resource -> Word32 -> Ptr Message -> Ptr Argument|]

makeDispatcherForeigns :: String -> TH.Name -> Scan [TH.Dec]
makeDispatcherForeigns str name = do
  forExport <- TH.ForeignD . TH.ExportF TH.CCall cName name <$> dispatcherType
  forImport <- TH.ForeignD . TH.ImportF TH.CCall TH.Safe ('&' : cName) importName <$> importType
  pure [forExport, forImport]
 where
  cName = "s_" ++ str ++ "Dispatcher"
  importType = [t|FunPtr $dispatcherType|]
  importName = TH.mkName $ str ++ "DispatcherPtr"

makeSetterType :: TH.Name -> Scan TH.Type
makeSetterType name = [t|Ptr Resource -> $(TH.conT name) -> IO () -> IO ()|]

makeSetterBody :: TH.Name -> Scan TH.Clause
makeSetterBody dispName =
  let resName = TH.mkName "resource"
      implName = TH.mkName "impl"
      destroyName = TH.mkName "destroy"
      body =
        TH.normalB $
          TH.appE (TH.appE (TH.appE [e|setResourceDispatcher $(TH.varE resName)|] (TH.varE implName)) (TH.varE dispName)) (TH.varE destroyName)
   in TH.clause [TH.varP resName, TH.varP implName, TH.varP destroyName] body []

makeDispatcher :: String -> [(String, [ArgumentType])] -> Scan [TH.Dec]
makeDispatcher name xs = do
  dataType@(TH.DataD _ dataName [] Nothing [TH.RecC _ fs] []) <- makeDispatchRecord name xs
  let clause = makeDispatchClause (dataName, map (\(n, _, _) -> n) fs) $ map snd xs
  setterSig <- TH.sigD setterName (makeSetterType dataName)
  setterFun <- TH.funD setterName [makeSetterBody $ TH.mkName $ name ++ "DispatcherPtr"]
  foreigns <- makeDispatcherForeigns name dispName
  dispatcherSig <- TH.sigD dispName dispatcherType
  dispatcherFun <- TH.funD dispName [clause]
  pure $ dataType : [setterSig, setterFun] ++ foreigns ++ [dispatcherSig, dispatcherFun]
 where
  dispName = TH.mkName $ name ++ "Dispatcher"
  setterName = TH.mkName $ "set" ++ cleanName name ++ "Dispatcher"

foreign import ccall unsafe "wl_resource_set_dispatcher"
  c_set_dispatcher ::
    Ptr Resource ->
    FunPtr (Ptr () -> Ptr Resource -> Word32 -> Ptr Message -> Ptr Argument -> IO ()) ->
    -- | Implementation
    Ptr () ->
    -- | Data
    Ptr () ->
    FunPtr (Ptr Resource -> IO ()) ->
    IO ()

foreign import ccall "wrapper" mkDestroyHandler :: (Ptr Resource -> IO ()) -> IO (FunPtr (Ptr Resource -> IO ()))

setResourceDispatcher ::
  Ptr Resource ->
  a ->
  FunPtr (Ptr () -> Ptr Resource -> Word32 -> Ptr Message -> Ptr Argument -> IO ()) ->
  IO () ->
  IO ()
setResourceDispatcher resource handlers dispatcher destroy = do
  sPtr <- newStablePtr handlers
  ref <- newIORef undefined
  destroyPointer <- mkDestroyHandler $ \_ -> do
    freeStablePtr sPtr
    freeHaskellFunPtr =<< readIORef ref
    destroy
  writeIORef ref destroyPointer
  c_set_dispatcher
    resource
    dispatcher
    (castStablePtrToPtr sPtr)
    nullPtr
    destroyPointer
