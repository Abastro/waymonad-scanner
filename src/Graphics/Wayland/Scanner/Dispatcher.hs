{-# LANGUAGE TemplateHaskellQuotes #-}

module Graphics.Wayland.Scanner.Dispatcher (
  makeDispatcher,
)
where

import Control.Monad
import Foreign.StablePtr (castPtrToStablePtr, deRefStablePtr, freeStablePtr, newStablePtr)

import Graphics.Wayland.Scanner.Marshal
import Graphics.Wayland.Scanner.Types
import Graphics.Wayland.Scanner.WLS
import Graphics.Wayland.Server.Resource (Resource, resourceSetDispatcher)
import Graphics.Wayland.Util.Types (Dispatcher)
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
    TH.varBangType (naming.requestFieldOf request) $ TH.bangType (pure bang) (argsToIOType args [t|()|])

marshalNameOf :: (Show n) => n -> TH.Name
marshalNameOf idx = TH.mkName ("marshal" ++ show idx)

dispatchExp :: InterfaceNaming -> TH.Name -> TH.Name -> TH.Name -> [String] -> Scan TH.Exp
dispatchExp naming implPtr opcode args requests =
  TH.doE
    [ TH.bindS reqImplBinds [e|deRefStablePtr (castPtrToStablePtr $(TH.varE implPtr))|],
      TH.noBindS $ TH.caseE (TH.varE opcode) $ zipWith matchFor requests [0 ..] <> [matchDefault]
    ]
 where
  reqImpl = TH.mkName "requestImpl"
  reqImplBinds = TH.sigP (TH.varP reqImpl) (TH.conT naming.requestRecord)

  matchDefault = TH.match TH.wildP (TH.normalB [e|pure ()|]) []
  matchFor request index = TH.match (TH.litP $ TH.integerL index) (TH.normalB matchExp) []
   where
    matchExp = [e|$(TH.varE $ marshalNameOf index) $(TH.varE args) $ $(TH.varE field) $(TH.varE reqImpl)|]
    field = naming.requestFieldOf request

-- Generates 'dispatch :: Ptr () -> Resource -> Word32 -> Message -> Ptr Argument -> IO ()'.
-- Ptr () denotes the implementation.
dispatchFnDec :: InterfaceNaming -> [(String, [ArgumentType])] -> Scan [TH.Dec]
dispatchFnDec naming requests = do
  marshalDecs <- fmap undefined <$> zipWithM makeMarshaller (marshalNameOf <$> [0 :: Int ..]) (snd <$> requests)
  sig <- TH.sigD naming.dispatcher [t|Dispatcher Resource|]
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

-- Generates 'setDispatcher :: Resource -> {RequestRecord} -> (Resource -> IO ()) -> IO ()'
setDispatchFnDec :: InterfaceNaming -> Scan [TH.Dec]
setDispatchFnDec naming = do
  sig <- TH.sigD setterName [t|Resource -> $(TH.conT naming.requestRecord) -> (Resource -> IO ()) -> IO ()|]
  fun <- TH.funD setterName [TH.clause [TH.varP resource] (TH.normalB bodyExpr) []]
  pure [sig, fun]
 where
  setterName = TH.mkName . hsVarName $ "set_" ++ naming.interfaceName ++ "_dispatcher"
  bodyExpr = [e|setDispatcherForResource $(TH.varE resource) $(TH.varE naming.dispatcher)|]
  resource = TH.mkName "resource"

makeDispatcher :: String -> [(String, [ArgumentType])] -> Scan [TH.Dec]
makeDispatcher interfaceName requests = do
  dataType <- makeDispatchRecord naming requests
  setters <- setDispatchFnDec naming
  dispatchers <- dispatchFnDec naming requests
  pure $ dataType : setters ++ dispatchers
 where
  naming =
    InterfaceNaming
      { interfaceName,
        requestRecord = TH.mkName . hsConstrName $ interfaceName ++ "_requests",
        requestFieldOf = \field -> TH.mkName . hsVarName $ interfaceName ++ "_" ++ field,
        dispatcher = TH.mkName . hsVarName $ interfaceName ++ "_dispatcher",
        dispatcherPtr = TH.mkName . hsVarName $ interfaceName ++ "_dispatcher_ptr"
      }

setDispatcherForResource :: Resource -> Dispatcher Resource -> a -> (Resource -> IO ()) -> IO ()
setDispatcherForResource resource dispatcher handler onDestroy = do
  handlerPtr <- newStablePtr handler
  resourceSetDispatcher resource dispatcher handlerPtr $ \res -> do
    freeStablePtr handlerPtr
    onDestroy res
