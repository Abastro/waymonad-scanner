{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Graphics.Wayland.Scanner.Marshal (
  postEventFnDec,
  makeMarshaller,
  argsToIOType,
) where

import Control.Monad.Cont
import Data.Bifunctor
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BS
import Data.Foldable
import Data.Text qualified as T
import Data.Text.Encoding qualified as E
import Foreign
import System.Posix.Types (Fd)

import Graphics.Wayland.Scanner.Types
import Graphics.Wayland.Scanner.WLS
import Graphics.Wayland.Server.Resource
import Graphics.Wayland.Util.Types

import Language.Haskell.TH qualified as TH

-- | Types which support the conversion to Argument.
class AsArgument t where
  withArg :: t -> (Argument -> IO a) -> IO a
  peekArg :: Argument -> IO t

instance AsArgument T.Text where
  withArg :: T.Text -> (Argument -> IO a) -> IO a
  withArg txt act = BS.useAsCString (E.encodeUtf8 txt) (act . ptrToArgument)
  peekArg :: Argument -> IO T.Text
  peekArg arg = E.decodeUtf8 <$> BS.unsafePackCString (argumentToPtr arg)

instance AsArgument WlArray where
  withArg :: WlArray -> (Argument -> IO a) -> IO a
  withArg array act = with array (act . ptrToArgument)
  peekArg :: Argument -> IO WlArray
  peekArg arg = peek (argumentToPtr arg)

instance AsArgument Word32 where
  withArg :: Word32 -> (Argument -> IO a) -> IO a
  withArg num act = act $ wordToArgument (fromIntegral num)
  peekArg :: Argument -> IO Word32
  peekArg arg = pure $ fromIntegral (argumentToWord arg)

instance AsArgument Int32 where
  withArg :: Int32 -> (Argument -> IO a) -> IO a
  withArg num act = act $ wordToArgument (fromIntegral num)
  peekArg :: Argument -> IO Int32
  peekArg arg = pure $ fromIntegral (argumentToWord arg)

instance AsArgument Fd where
  withArg :: Fd -> (Argument -> IO a) -> IO a
  withArg fd act = act $ wordToArgument (fromIntegral fd)
  peekArg :: Argument -> IO Fd
  peekArg arg = pure $ fromIntegral (argumentToWord arg)

instance (AsArgument t) => AsArgument (Maybe t) where
  withArg :: (AsArgument t) => Maybe t -> (Argument -> IO a) -> IO a
  withArg = \case
    Just val -> withArg val
    Nothing -> \act -> act $ ptrToArgument nullPtr
  peekArg :: (AsArgument t) => Argument -> IO (Maybe t)
  peekArg arg = case argumentToPtr arg of
    n | n == nullPtr -> pure Nothing
    _ -> Just <$> peekArg arg

withArgCont :: (AsArgument t) => t -> ContT r IO Argument
withArgCont arg = ContT (withArg arg)

argTypeOf :: ArgumentType -> Scan TH.Type
argTypeOf = \case
  IntArg -> [t|Int32|]
  UIntArg -> [t|Word32|]
  FixedArg -> error "not supported"
  StringArg canNull -> applyNullable canNull [t|Maybe T.Text|]
  ObjectArg canNull objName -> applyNullable canNull (getType objName)
  NewIdArg canNull _ -> applyNullable canNull [t|Word32|]
  ArrayArg canNull -> applyNullable canNull [t|WlArray|]
  FdArg -> [t|Fd|]
 where
  getType objName = (\info -> info.objType) <$> getObjectConvert objName
  applyNullable = \case
    NonNull -> id
    Nullable -> \typ -> [t|Maybe $typ|]

-- | Gives the type {Arguments} -> IO ().
argsToIOType :: [ArgumentType] -> Scan TH.Type -> Scan TH.Type
argsToIOType argTypes retType =
  foldr (\l r -> [t|$l -> $r|]) [t|IO $retType|] $ argTypeOf <$> argTypes

demarshallArgExp :: Scan TH.Exp -> Scan TH.Exp -> ArgumentType -> TH.Code Scan (ContT () IO Argument)
demarshallArgExp client arg = \case
  -- Relies on the fact that fromIntegral preserves the bits.
  IntArg -> [||withArgCont @Int32 $$(TH.unsafeCodeCoerce arg)||]
  UIntArg -> [||withArgCont @Word32 $$(TH.unsafeCodeCoerce arg)||]
  FixedArg -> error "not supported"
  StringArg NonNull -> [||withArgCont @T.Text $$(TH.unsafeCodeCoerce arg)||]
  StringArg Nullable -> [||withArgCont @(Maybe T.Text) $$(TH.unsafeCodeCoerce arg)||]
  -- TODO Nullable ctrl
  ObjectArg canNull objName -> TH.bindCode ((\info -> info.objConvert1) <$> getObjectConvert objName) $
    \convert ->
      [||
      lift $ $$(TH.unsafeCodeCoerce $ pure convert) $$(TH.unsafeCodeCoerce client) $$(TH.unsafeCodeCoerce arg)
      ||]
  -- TODO Nullable ctrl
  NewIdArg canNull _ -> [||withArgCont @Word32 $$(TH.unsafeCodeCoerce arg)||]
  ArrayArg NonNull -> [||withArgCont @WlArray $$(TH.unsafeCodeCoerce arg)||]
  ArrayArg Nullable -> [||withArgCont @(Maybe WlArray) $$(TH.unsafeCodeCoerce arg)||]
  FdArg -> [||withArgCont @Fd $$(TH.unsafeCodeCoerce arg)||]

postEventExp ::
  Scan TH.Exp ->
  Scan TH.Exp ->
  [(Scan TH.Exp, ArgumentType)] ->
  Word32 ->
  TH.Code Scan (IO ())
postEventExp client target args opcode =
  [||
  (`runContT` pure) $ do
    argList <- sequenceA $$demarshalled
    argsPtr <- ContT (withArray argList)
    lift $ resourcePostEventArray $$(TH.unsafeCodeCoerce target) opcode argsPtr
  ||]
 where
  demarshalled =
    TH.unsafeCodeCoerce . TH.listE . fmap TH.unTypeCode $
      uncurry (demarshallArgExp client) <$> args

postEventFnDec :: TH.Name -> [ArgumentType] -> Integer -> Scan [TH.Dec]
postEventFnDec fnName argTypes opcode = do
  sig <- TH.sigD fnName [t|Resource -> $(argsToIOType argTypes [t|()|])|]
  fun <- TH.funD fnName [TH.clause (TH.varP target : argPatterns) (TH.normalB bodyExpr) []]
  pure [sig, fun]
 where
  bodyExpr =
    TH.doE
      [ TH.bindS (TH.varP client) [e|resourceGetClient $(TH.varE target)|],
        TH.noBindS $ TH.unTypeCode $ postEventExp (TH.varE client) (TH.varE target) argExps (fromIntegral opcode)
      ]
  target = TH.mkName "target"
  client = TH.mkName "client"

  args = zip (argNameOf <$> [0 :: Int ..]) argTypes
  argNameOf idx = TH.mkName ("arg" <> show idx)
  argPatterns = TH.varP . fst <$> args
  argExps = first TH.varE <$> args

marshallArgExp :: TH.Code Scan Argument -> ArgumentType -> Scan TH.Exp
marshallArgExp arg = \case
  IntArg -> TH.unTypeCode [||peekArg @Int32 $$arg||]
  UIntArg -> TH.unTypeCode [||peekArg @Word32 $$arg||]
  FixedArg -> error "Can't decode fixed point values yet"
  StringArg NonNull -> TH.unTypeCode [||peekArg @T.Text $$arg||]
  StringArg Nullable -> TH.unTypeCode [||peekArg @(Maybe T.Text) $$arg||]
  ObjectArg canNull str -> do
    convert <- (\info -> info.objConvert) <$> getObjectConvert str
    TH.unTypeCode [||$$(TH.unsafeCodeCoerce $ pure convert) (argumentToPtr @Resource $$arg)||]
  NewIdArg canNull _ -> TH.unTypeCode [||peekArg @Word32 $$arg||]
  ArrayArg NonNull -> TH.unTypeCode [||peekArg @WlArray $$arg||]
  ArrayArg Nullable -> TH.unTypeCode [||peekArg @(Maybe WlArray) $$arg||]
  FdArg -> TH.unTypeCode [||peekArg @Fd $$arg||]

-- ? Reduce the amount of untyped code
makeMarshalExp :: TH.Name -> TH.Name -> [ArgumentType] -> Scan TH.Exp
makeMarshalExp argPtr handler argTypes =
  TH.doE
    [ TH.bindS (TH.listP argPatterns) $ TH.unTypeCode [||peekArray @Argument numArgs $$argPtrE||],
      TH.noBindS $ foldl' (\l r -> [e|$l <*> $r|]) [e|pure $(TH.varE handler)|] marshalled
    ]
 where
  argPtrE = TH.unsafeCodeCoerce $ TH.varE argPtr
  numArgs = length argTypes
  argNameOf idx = TH.mkName ("arg" ++ show idx)
  args = zip (argNameOf <$> [0 :: Int ..]) argTypes
  argPatterns = TH.varP . fst <$> args
  argExps = first (TH.unsafeCodeCoerce . TH.varE) <$> args
  marshalled = uncurry marshallArgExp <$> argExps

-- TH.clause [if null xs then TH.wildP else TH.varP mpName, TH.varP funName] body []

makeMarshaller :: TH.Name -> [ArgumentType] -> Scan [TH.Dec]
makeMarshaller name argTypes = do
  sig <- TH.sigD name [t|Ptr Argument -> $(argsToIOType argTypes [t|()|]) -> IO ()|]
  fun <- TH.funD name [theClause]
  pure [sig, fun]
 where
  theClause =
    if null argTypes
      then TH.clause [TH.wildP, TH.varP handler] (TH.normalB $ TH.varE handler) []
      else TH.clause [TH.varP argPtr, TH.varP handler] (TH.normalB $ makeMarshalExp argPtr handler argTypes) []
  argPtr = TH.mkName "argPtr"
  handler = TH.mkName "handler"
