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

data InterfaceNaming = InterfaceNaming
  { interfaceName :: String,
    requestRecord :: TH.Name,
    requestFieldOf :: String -> TH.Name,
    dispatcher :: TH.Name,
    dispatcherPtr :: TH.Name
  }

makeDispatchRecord :: InterfaceNaming -> [(String, [ArgumentType])] -> Scan TH.Dec
makeDispatchRecord naming requests = TH.dataD (pure []) naming.requestRecord [] Nothing [TH.recC naming.requestRecord (makeField <$> requests)] []
 where
  bang = TH.Bang TH.NoSourceUnpackedness TH.SourceStrict
  makeField (request, args) =
    TH.varBangType (naming.requestFieldOf request) $ TH.bangType (pure bang) (argsToIOType args)

dispatcherType :: Scan TH.Type
dispatcherType = [t|Ptr () -> Ptr Resource -> Word32 -> Ptr Message -> Ptr Argument -> IO ()|]

makeDispatcherForeigns :: InterfaceNaming -> Scan [TH.Dec]
makeDispatcherForeigns naming = do
  forExport <- TH.ForeignD . TH.ExportF TH.CCall cName naming.dispatcher <$> dispatcherType
  forImport <- TH.ForeignD . TH.ImportF TH.CCall TH.Safe ('&' : cName) naming.dispatcherPtr <$> importType
  pure [forExport, forImport]
 where
  cName = "s_" ++ naming.interfaceName ++ "Dispatcher"
  importType = [t|FunPtr $dispatcherType|]

marshalNameOf :: (Show n) => n -> TH.Name
marshalNameOf idx = TH.mkName ("marshal" ++ show idx)

dispatchExp :: InterfaceNaming -> TH.Name -> TH.Name -> TH.Name -> [String] -> Scan TH.Exp
dispatchExp naming implPtr opcode args requests =
  TH.doE
    [ TH.bindS reqImplBinds [e|deRefStablePtr (castPtrToStablePtr $(TH.varE implPtr))|],
      TH.noBindS $ TH.caseE (TH.varE opcode) $ zipWith matchFor requests [0 ..] <> [matchDef]
    ]
 where
  reqImpl = TH.mkName "requestImpl"
  reqImplBinds = TH.sigP (TH.varP reqImpl) (TH.conT naming.requestRecord)

  matchDef = TH.match TH.wildP (TH.normalB [e|pure ()|]) []
  matchFor request index = TH.match (TH.litP $ TH.integerL index) (TH.normalB matchExp) []
   where
    matchExp = [e|$(TH.varE $ marshalNameOf index) $(TH.varE args) $ $(TH.varE field) $(TH.varE reqImpl)|]
    field = naming.requestFieldOf request

dispatchFnDec :: InterfaceNaming -> [(String, [ArgumentType])] -> Scan [TH.Dec]
dispatchFnDec naming requests = do
  marshalDecs <- fmap undefined <$> zipWithM makeMarshaller (marshalNameOf <$> [0 :: Int ..]) (snd <$> requests)
  sig <-
    TH.sigD
      naming.dispatcher
      [t|Ptr () -> Ptr Resource -> Word32 -> Ptr Message -> Ptr Argument -> IO ()|]
  fun <-
    TH.funD
      naming.dispatcher
      [TH.clause [TH.varP implPtr, TH.wildP, TH.varP opcode, TH.wildP, TH.varP args] (TH.normalB funExp) (concat marshalDecs)]
  pure [sig, fun]
 where
  funExp = dispatchExp naming implPtr opcode args (fst <$> requests)

  implPtr = TH.mkName "implPtr"
  opcode = TH.mkName "opcode"
  args = TH.mkName "args"

setDispatchFnDec :: InterfaceNaming -> Scan [TH.Dec]
setDispatchFnDec naming = do
  sig <- TH.sigD setterName [t|Ptr Resource -> $(TH.conT naming.requestRecord) -> IO () -> IO ()|]
  fun <- TH.funD setterName [TH.clause [TH.varP resource, TH.varP impl, TH.varP destroy] (TH.normalB bodyExpr) []]
  pure [sig, fun]
 where
  setterName = TH.mkName $ "set" ++ hsConstrName naming.interfaceName ++ "Dispatcher"
  bodyExpr =
    [e|
      setResourceDispatcher $(TH.varE resource) $(TH.varE impl) $(TH.varE naming.dispatcherPtr) $(TH.varE destroy)
      |]
  resource = TH.mkName "resource"
  impl = TH.mkName "impl"
  destroy = TH.mkName "destroy"

makeDispatcher :: String -> [(String, [ArgumentType])] -> Scan [TH.Dec]
makeDispatcher interfaceName requests = do
  dataType <- makeDispatchRecord naming requests
  setters <- setDispatchFnDec naming
  foreigns <- makeDispatcherForeigns naming
  dispatchers <- dispatchFnDec naming requests
  pure $ dataType : setters ++ foreigns ++ dispatchers
 where
  naming =
    InterfaceNaming
      { interfaceName,
        requestRecord = TH.mkName . hsConstrName $ interfaceName ++ "_requests",
        requestFieldOf = \field -> TH.mkName . hsVarName $ interfaceName ++ "_" ++ field,
        dispatcher = TH.mkName . hsVarName $ interfaceName ++ "_dispatcher",
        dispatcherPtr = TH.mkName . hsVarName $ interfaceName ++ "_dispatcher_ptr"
      }

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
