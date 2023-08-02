{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Graphics.Wayland.Scanner.Marshal (
  makeMarshaller,
  takerType,
  makePostFun,
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
import Graphics.Wayland.Server.Resource (Resource, resourceGetClient)
import Graphics.Wayland.Util.Types (WlArgument, WlArray (..), unArray)

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

withStringArg :: Ptr CString -> Maybe T.Text -> IO () -> IO ()
withStringArg ptr Nothing act = poke ptr nullPtr >> act
withStringArg ptr (Just txt) act =
  BS.useAsCString (E.encodeUtf8 txt) $ \cStr -> do
    poke ptr cStr
    act

withArrayArg :: Ptr (Ptr WlArray) -> Maybe BS.ByteString -> IO () -> IO ()
withArrayArg ptr Nothing act = poke ptr nullPtr >> act
withArrayArg ptr (Just array) act = with (WlArray array) $ \aPtr -> do
  poke ptr aPtr
  act

-- | Generates:
-- Writes given value to the argument pointer, and run the parameter action.
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
        resourcePtr <- $(pure convert) theClient $e
        poke $p resourcePtr
        act
      |]
{- argTypeToDemarshalExp (NullableObjectArg str) _ p e = do
  convert <- (\(_, _, v) -> v) <$> getObjectConvert str
  let rpName = TH.mkName "resourcePtr"
      unMaybe = TH.AppE (TH.VarE 'fromMaybe) (TH.VarE 'nullPtr)
  pure $
    TH.DoE
      [ TH.BindS (TH.VarP rpName) $ TH.AppE (TH.AppE (TH.VarE 'traverse) convert) e,
        TH.NoBindS $ TH.AppE (TH.VarE '(>>)) (TH.AppE (TH.AppE (TH.VarE 'poke) p) (TH.AppE unMaybe $ TH.VarE rpName))
      ]
      -}
argTypeToDemarshalExp _ _ p e = pure [e|(>>) (poke $p $e)|]

foreign import ccall "wl_resource_post_event_array" c_post :: Ptr Resource -> Word32 -> Ptr WlArgument -> IO ()

doActionsExp :: (TH.Quote mo, Monad mi) => [TH.Code mo (mi ())] -> TH.Code mo (mi ())
doActionsExp exprs =
  TH.unsafeCodeCoerce $ TH.doE (TH.noBindS . TH.unTypeCode <$> exprs)

knownLambda :: (TH.Quote mo) => TH.Name -> TH.Code mo a -> TH.Code mo (r -> a)
knownLambda bindName expr =
  TH.unsafeCodeCoerce $ TH.lamE [TH.varP bindName] (TH.unTypeCode expr)

pokeStringArg :: Int -> Ptr CString -> Maybe T.Text -> ContT () IO ()
pokeStringArg offset ptr = \case
  Nothing -> lift $ pokeByteOff ptr offset nullPtr
  Just txt -> do
    cStr <- ContT $ BS.useAsCString (E.encodeUtf8 txt)
    lift $ pokeByteOff ptr offset cStr

-- Can be casted from 'Ptr WlArgument', and all its alignment is of pointer-size.
pokeArrayArg :: Int -> Ptr (Ptr WlArray) -> Maybe BS.ByteString -> ContT () IO ()
pokeArrayArg offset ptr = \case
  Nothing -> lift $ pokeByteOff ptr offset nullPtr
  Just array -> do
    aPtr <- ContT $ with (WlArray array)
    lift $ pokeByteOff ptr offset aPtr -- Just poking bytes

instance Storable WlArgument

postEventExp ::
  TH.Code Scan (Ptr Resource) ->
  [(TH.Name, ArgumentType)] ->
  Integer ->
  TH.Code Scan (IO ())
postEventExp targetPtr args opcode =
  [||allocaArray @WlArgument numArgs $ $$withArgsPtr||]
 where
  withArgsPtr = knownLambda argsName [||(`runContT` pure) $ $$(doActionsExp actions)||]

  numArgs = length args
  argsName = TH.mkName "argumentsPtr"
  argsPtr = TH.unsafeCodeCoerce $ TH.varE argsName -- Was 8 * number
  opcodeW32 :: Word32 = fromIntegral opcode

  actions :: [TH.Code Scan (ContT () IO ())]
  actions =
    zipWith marshallArg [0 ..] args
      <> pure [||lift $ c_post $$targetPtr opcodeW32 $$argsPtr||]

  marshallArg :: Int -> (TH.Name, ArgumentType) -> TH.Code Scan (ContT () IO ())
  marshallArg = undefined

makePostClause :: (Monad m) => [ArgumentType] -> Integer -> Scanner m TH.ClauseQ
makePostClause xs opcode = do
  exps <- zipWithM deMarshalExps (zip xs [0 ..]) argNames
  let lam = TH.lamE [TH.varP apName] $ foldr TH.appE act exps
  pure $ TH.clause (TH.varP rpName : fmap TH.varP argNames) (TH.normalB [e|$callocExp $lam|]) []
 where
  apName = TH.mkName "argumentsPtr"
  rpName = TH.mkName "targetPtr"
  argNames = take (length xs) $ map (TH.mkName . ("arg" ++) . show) [0 :: Int ..]
  callocExp = [e|allocaBytes $(TH.litE (TH.IntegerL $ 8 * fromIntegral (length xs)))|]
  deMarshalExps (arg, i) argName =
    argTypeToDemarshalExp
      arg
      (TH.varE rpName)
      [e|plusPtr $(TH.varE apName) $(TH.litE . TH.IntegerL $ i * 8)|]
      (TH.varE argName)
  act = [e|c_post $(TH.varE rpName) opcode $(TH.varE apName)|]

makePostFun :: (Monad m) => TH.Name -> [ArgumentType] -> Integer -> Scanner m [TH.DecQ]
makePostFun name xs opcode = do
  clause <- makePostClause xs opcode
  tType <- takerType xs
  let funType = [t|Ptr Resource -> $tType|]
  pure [TH.sigD name funType, TH.funD name [clause]]

peekStringArg :: Ptr CString -> IO T.Text
peekStringArg ptr = do
  strPtr <- peek ptr
  bs <- BS.unsafePackCString strPtr
  pure (E.decodeUtf8 bs)

peekArrayArg :: Ptr (Ptr WlArray) -> IO BS.ByteString
peekArrayArg ptr = do
  arrayPtr <- peek ptr
  array <- peek arrayPtr
  pure (unArray array)

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
  let funType = [t|Ptr WlArgument -> $tType -> IO ()|]
  clause <- makeMarshalClause xs
  pure [TH.sigD name funType, TH.funD name [clause]]
