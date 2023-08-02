{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Graphics.Wayland.Scanner.Marshal (
  postEventFnDec,
  makeMarshaller,
  takerType,
) where

import Control.Monad
import Control.Monad.Cont
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BS
import Data.Foldable
import Data.Text qualified as T
import Data.Text.Encoding qualified as E
import Foreign
import Foreign.C.String (CString)
import System.Posix.Types (Fd)

import Graphics.Wayland.Scanner.Types
import Graphics.Wayland.Scanner.WLS
import Graphics.Wayland.Server.Resource
import Graphics.Wayland.Util.Types

import Data.Bifunctor
import Language.Haskell.TH qualified as TH

-- ? Try typed template haskell
-- TODO What is this arcane machinary

nullableType :: CanNull -> TH.TypeQ -> TH.TypeQ
nullableType = \case
  NonNull -> id
  Nullable -> \typ -> [t|Maybe $typ|]

argTypeToExp :: (Monad m) => ArgumentType -> Scanner m TH.TypeQ
argTypeToExp = \case
  IntArg -> pure [t|Int32|]
  UIntArg -> pure [t|Word32|]
  FixedArg -> error "Can't handle fixed point arguments yet"
  StringArg canNull -> pure $ nullableType canNull [t|T.Text|]
  ObjectArg canNull str -> nullableType canNull . pure . (\info -> info.objType) <$> getObjectConvert str
  NewIdArg canNull _ -> pure $ nullableType canNull [t|Word32|]
  ArrayArg canNull -> pure $ nullableType canNull [t|BS.ByteString|]
  FdArg -> pure [t|Fd|]

decodePattern :: (Monad m) => ArgumentType -> TH.Name -> Scanner m TH.PatQ
decodePattern arg name = fmap (TH.SigP (TH.VarP name)) <$> argTypeToExp arg

{-
argTypeToDemarshalExp :: (Monad m) => ArgumentType -> TH.ExpQ -> TH.ExpQ -> TH.ExpQ -> Scanner m TH.ExpQ
argTypeToDemarshalExp FixedArg _ _ _ = error "Can't encode fixed point values yet"
argTypeToDemarshalExp (StringArg canNull) _ p e = pure [e|withStringArg $p (Just $e)|]
-- argTypeToDemarshalExp NullableStringArg _ p e = [e|withStringArg $(pure p) $(pure e)|]
argTypeToDemarshalExp (ArrayArg canNull) _ p e = pure [e|withArrayArg $p (Just $e)|]
-- argTypeToDemarshalExp NullableArrayArg _ p e = [e|withArrayArg $(pure p) $(pure e)|]
argTypeToDemarshalExp (ObjectArg canNull str) t p e = do
  convert <- (\info -> info.objConvert1) <$> getObjectConvert str
  pure
    [e|
      \act -> do
        theClient <- resourceGetClient $t
        argResPtr <- $(pure convert) theClient $e
        poke $p argResPtr
        act
      |]
argTypeToDemarshalExp (NullableObjectArg str) _ p e = do
  convert <- (\(_, _, v) -> v) <$> getObjectConvert str
  let rpName = TH.mkName "resourcePtr"
      unMaybe = TH.AppE (TH.VarE 'fromMaybe) (TH.VarE 'nullPtr)
  pure $
    TH.DoE
      [ TH.BindS (TH.VarP rpName) $ TH.AppE (TH.AppE (TH.VarE 'traverse) convert) e,
        TH.NoBindS $ TH.AppE (TH.VarE '(>>)) (TH.AppE (TH.AppE (TH.VarE 'poke) p) (TH.AppE unMaybe $ TH.VarE rpName))
      ]
argTypeToDemarshalExp _ _ p e = pure [e|(>>) (poke $p $e)|]
-}

-- | Types which support the conversion to Argument.
class AsArgument t where
  withArg :: t -> (Argument -> IO a) -> IO a

instance AsArgument T.Text where
  withArg :: T.Text -> (Argument -> IO a) -> IO a
  withArg txt act = BS.useAsCString (E.encodeUtf8 txt) (act . ptrToArgument)

instance AsArgument WlArray where
  withArg :: WlArray -> (Argument -> IO a) -> IO a
  withArg array act = with array (act . ptrToArgument)

instance AsArgument Word32 where
  withArg :: Word32 -> (Argument -> IO a) -> IO a
  withArg num act = act $ wordToArgument (fromIntegral num)

instance AsArgument Int32 where
  withArg :: Int32 -> (Argument -> IO a) -> IO a
  withArg num act = act $ wordToArgument (fromIntegral num)

instance AsArgument Fd where
  withArg :: Fd -> (Argument -> IO a) -> IO a
  withArg fd act = act $ wordToArgument (fromIntegral fd)

instance (AsArgument t) => AsArgument (Maybe t) where
  withArg :: (AsArgument t) => Maybe t -> (Argument -> IO a) -> IO a
  withArg = \case
    Just val -> withArg val
    Nothing -> \act -> act $ ptrToArgument nullPtr

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
argsToIOType :: [ArgumentType] -> Scan TH.Type
argsToIOType argTyps =
  foldr (\l r -> [t|$l -> $r|]) [t|IO ()|] $ argTypeOf <$> argTyps

demarshallArgExp :: Scan TH.Exp -> Scan TH.Exp -> ArgumentType -> TH.Code Scan (ContT () IO Argument)
demarshallArgExp client arg = \case
  -- Relies on the fact that fromIntegral preserves the bits.
  IntArg -> [||withArgCont @Int32 $$(TH.unsafeCodeCoerce arg)||]
  UIntArg -> [||withArgCont @Word32 $$(TH.unsafeCodeCoerce arg)||]
  FixedArg -> error "not supported"
  StringArg NonNull -> [||withArgCont @T.Text $$(TH.unsafeCodeCoerce arg)||]
  StringArg Nullable -> [||withArgCont @(Maybe T.Text) $$(TH.unsafeCodeCoerce arg)||]
  -- TODO Nullable ctrl
  ObjectArg _ objName -> TH.bindCode ((\info -> info.objConvert1) <$> getObjectConvert objName) $
    \convert ->
      [||
      lift $ $$(TH.unsafeCodeCoerce $ pure convert) $$(TH.unsafeCodeCoerce client) $$(TH.unsafeCodeCoerce arg)
      ||]
  -- TODO Nullable ctrl
  NewIdArg _ _ -> [||withArgCont @Word32 $$(TH.unsafeCodeCoerce arg)||]
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
    argList <- sequenceA $$demarshalls
    argsPtr <- ContT (withArray argList)
    lift $ resourcePostEventArray $$(TH.unsafeCodeCoerce target) opcode argsPtr
  ||]
 where
  demarshalls =
    TH.unsafeCodeCoerce . TH.listE . fmap TH.unTypeCode $
      uncurry (demarshallArgExp client) <$> args

postEventFnDec :: TH.Name -> [ArgumentType] -> Integer -> Scan [TH.Dec]
postEventFnDec fnName argTypes opcode = do
  signature <- TH.sigD fnName [t|Resource -> $handleType|]
  implementation <- TH.funD fnName [TH.clause (TH.varP target : argPatterns) (TH.normalB bodyExpr) []]
  pure [signature, implementation]
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
  handleType = argsToIOType argTypes
  argExps = first TH.varE <$> args

peekStringArg :: Ptr CString -> IO T.Text
peekStringArg ptr = do
  strPtr <- peek ptr
  E.decodeUtf8 <$> BS.unsafePackCString strPtr

peekArrayArg :: Ptr (Ptr WlArray) -> IO WlArray
peekArrayArg ptr = do
  arrayPtr <- peek ptr
  peek arrayPtr

argTypeToMarshalExp :: (Monad m) => ArgumentType -> TH.ExpQ -> Scanner m TH.ExpQ
argTypeToMarshalExp FixedArg _ = error "Can't decode fixed point values yet"
argTypeToMarshalExp (StringArg canNull) e = pure [e|peekStringArg $e|]
{-
argTypeToMarshalExp NullableStringArg e =
  pure $
    let ptrName = TH.mkName "strPtr"
        bsName = TH.mkName "bs"
        trueStmt = TH.AppE (TH.VarE 'pure) (TH.ConE 'Nothing)
        falseStmt =
          TH.DoE
            [ TH.BindS (TH.VarP bsName) $ TH.AppE (TH.VarE 'BS.unsafePackCString) (TH.VarE ptrName),
              TH.NoBindS (TH.AppE (TH.VarE 'pure) (TH.AppE (TH.ConE 'Just) $ TH.AppE (TH.VarE 'E.decodeUtf8) (TH.VarE bsName)))
            ]
     in TH.DoE
          [ TH.BindS (TH.VarP ptrName) $ TH.AppE (TH.VarE 'peek) e,
            TH.NoBindS $
              TH.CaseE
                (TH.AppE ((TH.AppE (TH.VarE '(==)) (TH.VarE 'nullPtr))) (TH.VarE ptrName))
                [ TH.Match (TH.ConP 'True []) (TH.NormalB trueStmt) [],
                  TH.Match (TH.ConP 'False []) (TH.NormalB falseStmt) []
                ]
          ]-}
argTypeToMarshalExp (ArrayArg canNull) e = pure [e|peekArrayArg $e|]
{-
argTypeToMarshalExp NullableArrayArg e =
  pure $
    let ptrName = TH.mkName "arrayPtr"
        arrName = TH.mkName "array"
        falseStmt =
          TH.DoE
            [ TH.BindS (TH.VarP arrName) $ TH.AppE (TH.VarE 'peek) (TH.VarE ptrName),
              TH.NoBindS (TH.AppE (TH.VarE 'pure) $ TH.AppE (TH.ConE 'Just) (TH.AppE (TH.VarE 'unArray) (TH.VarE arrName)))
            ]
        trueStmt = TH.AppE (TH.VarE 'pure) (TH.ConE 'Nothing)
     in TH.DoE
          [ TH.BindS (TH.VarP ptrName) $ TH.AppE (TH.VarE 'peek) e,
            TH.NoBindS $
              TH.CaseE
                (TH.AppE ((TH.AppE (TH.VarE '(==)) (TH.VarE 'nullPtr))) (TH.VarE ptrName))
                [ TH.Match (TH.ConP 'True []) (TH.NormalB trueStmt) [],
                  TH.Match (TH.ConP 'False []) (TH.NormalB falseStmt) []
                ]
          ]
          -}
argTypeToMarshalExp (ObjectArg canNull str) e = do
  convert <- (\info -> info.objConvert) <$> getObjectConvert str
  pure
    [e|
      do
        resourcePtr :: Ptr Resource <- peek $e
        $(pure convert) resourcePtr
      |]

{-
argTypeToMarshalExp (NullableObjectArg str) e = do
  convert <- (\(_, v, _) -> v) <$> getObjectConvert str
  let objPtr = TH.mkName "ptrName"
      falseStmt = TH.AppE (TH.AppE (TH.VarE 'fmap) (TH.ConE 'Just)) $ TH.AppE convert (TH.VarE objPtr)
      trueStmt = TH.AppE (TH.VarE 'pure) (TH.ConE 'Nothing)
  pure $
    TH.DoE
      [ TH.BindS (TH.SigP (TH.VarP objPtr) (TH.AppT (TH.ConT ''Ptr) (TH.ConT ''Resource))) $ TH.AppE (TH.VarE 'peek) e,
        TH.NoBindS $
          TH.CaseE
            (TH.AppE ((TH.AppE (TH.VarE '(==)) (TH.VarE 'nullPtr))) (TH.VarE objPtr))
            [ TH.Match (TH.ConP 'True []) (TH.NormalB trueStmt) [],
              TH.Match (TH.ConP 'False []) (TH.NormalB falseStmt) []
            ]
      ]
argTypeToMarshalExp (NullableNewIdArg _) e =
  pure $
    let objPtr = TH.mkName "ptrName"
        falseStmt = TH.AppE (TH.VarE 'pure) $ TH.AppE (TH.ConE 'Just) (TH.VarE objPtr)
        trueStmt = TH.AppE (TH.VarE 'pure) (TH.ConE 'Nothing)
     in TH.DoE
          [ TH.BindS (TH.VarP objPtr) $ TH.AppE (TH.VarE 'peek) e,
            TH.NoBindS $
              TH.CaseE
                (TH.AppE ((TH.AppE (TH.VarE '(==)) (TH.LitE $ TH.IntegerL 0))) (TH.VarE objPtr))
                [ TH.Match (TH.ConP 'True []) (TH.NormalB trueStmt) [],
                  TH.Match (TH.ConP 'False []) (TH.NormalB falseStmt) []
                ]
          ]-}
argTypeToMarshalExp _ e = pure [e|peek $e|]

argTypeToMarshal :: (Monad m) => ArgumentType -> TH.Name -> TH.ExpQ -> Scanner m TH.StmtQ
argTypeToMarshal at name e = TH.bindS <$> decodePattern at name <*> argTypeToMarshalExp at e

-- | Construct the type {Arguments} -> IO ().
takerType :: (Monad m) => [ArgumentType] -> Scanner m TH.TypeQ
takerType xs = do
  thTypes <- traverse argTypeToExp xs
  pure $ foldr (\l r -> [t|$l -> $r|]) [t|IO ()|] thTypes

makeMarshalBody :: (Monad m) => TH.Name -> TH.Name -> [ArgumentType] -> Scanner m TH.BodyQ
makeMarshalBody dataPtr funName xs = do
  argStmts <- zipWithM makeArgStmt (zip xs [0 ..]) argNames
  pure $ TH.normalB $ TH.doE (argStmts ++ [pure successCase])
 where
  argNames = take (length xs) $ map (TH.mkName . (++) "arg" . show) [0 :: Int ..]
  dataExp i = [e|plusPtr $(TH.varE dataPtr) $(TH.litE . TH.IntegerL $ i * 8)|]
  makeArgStmt (argtype, i) name = argTypeToMarshal argtype name (dataExp i)
  applyExp = foldl' TH.AppE (TH.VarE funName) $ fmap TH.VarE argNames
  successCase = TH.NoBindS applyExp

makeMarshalClause :: (Monad m) => [ArgumentType] -> Scanner m TH.ClauseQ
makeMarshalClause xs = do
  let mpName = TH.mkName "messagePtr"
  let funName = TH.mkName "takerFun"
  body <- makeMarshalBody mpName funName xs
  pure $ TH.clause [if null xs then TH.wildP else TH.varP mpName, TH.varP funName] body []

makeMarshaller :: (Monad m) => TH.Name -> [ArgumentType] -> Scanner m [TH.DecQ]
makeMarshaller name xs = do
  tType <- takerType xs
  let funType = [t|Ptr Argument -> $tType -> IO ()|]
  clause <- makeMarshalClause xs
  pure [TH.sigD name funType, TH.funD name [clause]]

marshallerFnDec :: TH.Name -> [ArgumentType] -> [Scan TH.Dec]
marshallerFnDec name argTypes =
  [ TH.sigD name [t|Ptr Argument -> $callbackType -> IO ()|],
    TH.funD name []
  ]
 where
  callbackType = argsToIOType argTypes
