{-# LANGUAGE TemplateHaskellQuotes #-}

module Graphics.Wayland.Scanner.Dispatcher
where

import Control.Monad
import Data.IORef
import Data.Word (Word32)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (FunPtr, Ptr, freeHaskellFunPtr, nullPtr)
import Foreign.StablePtr (castPtrToStablePtr, castStablePtrToPtr, deRefStablePtr, freeStablePtr, newStablePtr)

import Graphics.Wayland.Scanner.Marshal
import Graphics.Wayland.Util.Types (WlArgument, WlMessage)
import Graphics.Wayland.Scanner.Types
import Graphics.Wayland.Scanner.WLS
import Graphics.Wayland.Server.Resource (Client (..), Resource)
import Utility

import Language.Haskell.TH qualified as TH

makeDispatchClause :: (Monad m) => (TH.Name, [TH.Name]) -> [[ArgumentType]] -> Scanner m TH.Clause
makeDispatchClause (dataTName, fs) xs = do
  marshalDecs <- zipWithM makeMarshaller marshalNames xs
  let dataStmt = TH.BindS (TH.SigP (TH.VarP dataName) (TH.ConT dataTName)) $ TH.AppE (TH.VarE 'deRefStablePtr) $ TH.AppE (TH.VarE 'castPtrToStablePtr) (TH.VarE implName)
      makeMatch field index =
        let matchExp = TH.AppE (TH.AppE (TH.VarE $ makeMarshalName index) (TH.VarE argName)) (TH.AppE (TH.VarE field) (TH.VarE dataName))
         in TH.Match (TH.LitP (TH.IntegerL index)) (TH.NormalB matchExp) []
      failClause = TH.Match TH.WildP (TH.NormalB $ TH.AppE (TH.VarE 'pure) (TH.ConE '())) []
      clauses = zipWith makeMatch fs [0 ..] ++ [failClause]
      funExp = TH.DoE _ [dataStmt, TH.NoBindS (TH.CaseE (TH.VarE opName) clauses)]
  pure $ TH.Clause [TH.VarP implName, TH.WildP, TH.VarP opName, TH.WildP, TH.VarP argName] (TH.NormalB funExp) (concat marshalDecs)
 where
  implName = TH.mkName "implPtr"
  opName = TH.mkName "opcode"
  argName = TH.mkName "wlArgs"
  dataName = TH.mkName "takers"
  makeMarshalName = TH.mkName . (++) "marshal" . show
  marshalNames = take (length xs) $ fmap makeMarshalName [0 ..]

makeDispatchRecord :: (Monad m) => String -> [(String, [ArgumentType])] -> Scanner m TH.Dec
makeDispatchRecord name xs = do
  let dataName = TH.mkName $ cleanName name ++ "Requests"
      bang = TH.Bang TH.NoSourceUnpackedness TH.SourceStrict
      makeField (field, args) = do
        tType <- takerType args
        pure $ (TH.mkName (replaceUnder name ++ "Request" ++ cleanName field), bang, tType)
  dataCon <- TH.RecC dataName <$> mapM makeField xs
  pure $ TH.DataD [] dataName [] Nothing [dataCon] []

dispatcherType :: TH.Type
dispatcherType =
  let resType = TH.AppT (TH.ConT ''IO) (TH.TupleT 0)
      ptrType = TH.AppT (TH.ConT ''Ptr)
      thTypes = [ptrType (TH.TupleT 0), ptrType (TH.ConT ''Resource), TH.ConT ''Word32, ptrType (TH.ConT ''WlMessage), ptrType (TH.ConT ''WlArgument)]
   in foldr (\l r -> TH.AppT (TH.AppT TH.ArrowT l) r) resType thTypes

makeDispatcherForeigns :: String -> TH.Name -> [TH.Dec]
makeDispatcherForeigns str name =
  let cName = ("s_" ++ str ++ "Dispatcher")
      importType = TH.AppT (TH.ConT ''FunPtr) dispatcherType
      importName = TH.mkName $ str ++ "DispatcherPtr"
   in [ TH.ForeignD $ TH.ExportF TH.CCall cName name dispatcherType,
        TH.ForeignD $ TH.ImportF TH.CCall TH.Safe ('&' : cName) importName importType
      ]

makeSetterType :: TH.Name -> TH.Type
makeSetterType name =
  let resType = TH.AppT (TH.ConT ''IO) (TH.TupleT 0)
      ptrType = TH.AppT (TH.ConT ''Ptr)
      thTypes = [ptrType (TH.ConT ''Resource), TH.ConT name, resType]
   in foldr (\l r -> TH.AppT (TH.AppT TH.ArrowT l) r) resType thTypes

makeSetterBody :: TH.Name -> TH.Clause
makeSetterBody dispName =
  let resName = TH.mkName "resource"
      implName = TH.mkName "impl"
      destroyName = TH.mkName "destroy"
      body =
        TH.NormalB $
          TH.AppE (TH.AppE (TH.AppE (TH.AppE (TH.VarE 'setResourceDispatcher) (TH.VarE resName)) (TH.VarE implName)) (TH.VarE dispName)) (TH.VarE destroyName)
   in TH.Clause [TH.VarP resName, TH.VarP implName, TH.VarP destroyName] body []

makeDispatcher :: (Monad m, MonadFail m) => String -> [(String, [ArgumentType])] -> Scanner m [TH.Dec]
makeDispatcher name xs = do
  dataType@(TH.DataD _ dataName [] Nothing [TH.RecC _ fs] []) <- makeDispatchRecord name xs
  clause <- makeDispatchClause (dataName, map (\(n, _, _) -> n) fs) $ map snd xs
  let dispName = TH.mkName $ name ++ "Dispatcher"
      foreigns = makeDispatcherForeigns name dispName
      setterName = TH.mkName $ "set" ++ cleanName name ++ "Dispatcher"
      setterSig = TH.SigD setterName (makeSetterType dataName)
      setterFun = TH.FunD setterName [makeSetterBody $ TH.mkName $ name ++ "DispatcherPtr"]
      setter = [setterSig, setterFun]
      dispatcher = [TH.SigD dispName dispatcherType, TH.FunD dispName [clause]]
  pure $ dataType : setter ++ foreigns ++ dispatcher

foreign import ccall unsafe "wl_resource_set_dispatcher"
  c_set_dispatcher ::
    Ptr Resource ->
    FunPtr (Ptr () -> Ptr Resource -> Word32 -> Ptr WlMessage -> Ptr WlArgument -> IO ()) ->
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
  FunPtr (Ptr () -> Ptr Resource -> Word32 -> Ptr WlMessage -> Ptr WlArgument -> IO ()) ->
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

data WlInterface

foreign import ccall "wl_resource_create" c_create :: Ptr Client -> Ptr WlInterface -> CInt -> Word32 -> IO (Ptr Resource)

createResource :: Client -> Ptr WlInterface -> Int -> Word32 -> IO (Ptr Resource)
createResource (Client cPtr) iface version = c_create cPtr iface (fromIntegral version)
