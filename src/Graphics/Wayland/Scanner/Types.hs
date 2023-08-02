module Graphics.Wayland.Scanner.Types (
  CanNull (..),
  ArgumentType (..),
  argTypeFromData,
  WlEnum (..),
  WlRequest (..),
  WlEvent (..),
  Interface (..),
  WlProtocol (..),
)
where

data CanNull = NonNull | Nullable
  deriving (Show)

data ArgumentType
  = IntArg
  | UIntArg
  | FixedArg
  | StringArg CanNull
  | ObjectArg CanNull String
  | NewIdArg CanNull String
  | ArrayArg CanNull
  | FdArg
  deriving (Show)

argTypeFromData :: String -> Bool -> Maybe String -> ArgumentType
argTypeFromData "int" _ _ = IntArg
argTypeFromData "uint" _ _ = UIntArg
argTypeFromData "fixed" _ _ = FixedArg
argTypeFromData "string" False _ = StringArg NonNull
argTypeFromData "string" True _ = StringArg Nullable
argTypeFromData "object" False (Just s) = ObjectArg NonNull s
argTypeFromData "object" True (Just s) = ObjectArg Nullable s
argTypeFromData "new_id" False (Just s) = NewIdArg NonNull s
argTypeFromData "new_id" True (Just s) = NewIdArg Nullable s
argTypeFromData "array" False _ = ArrayArg NonNull
argTypeFromData "array" True _ = ArrayArg Nullable
argTypeFromData "fd" _ _ = FdArg
argTypeFromData x _ _ = error $ "Can't decode " ++ x ++ " as argument type"

newtype WlEnum = WlEnum [(String, Int)]
newtype WlRequest = WlRequest [(String, ArgumentType)]
newtype WlEvent = WlEvent [(String, ArgumentType)]

data Interface = Interface [(String, WlEnum)] [(String, WlRequest)] [(String, WlEvent)]

data WlProtocol = WlProtocol String [(String, Interface, Int)]
