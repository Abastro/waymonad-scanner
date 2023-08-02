{-# LANGUAGE TemplateHaskellQuotes #-}

module Graphics.Wayland.Scanner.WLS (
  ObjectInfo (..),
  Scan(..),
  ScannerEnv (..),
  getObjectConvert,
  scannerIO,
  runScanner,
)
where

import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Map (Map)
import Foreign.Ptr (Ptr)

import Data.Map qualified as M
import Graphics.Wayland.Server.Resource (Resource)
import Language.Haskell.TH qualified as TH

data ObjectInfo = ObjectInfo
  { objType :: !TH.Type,
    objConvert :: !TH.Exp,
    objConvert1 :: !TH.Exp
  }
type ObjectMap = Map String ObjectInfo

newtype ScannerEnv = ScannerEnv
  { scannerObjectMap :: ObjectMap
  }

newtype Scan a = Scan (ReaderT ScannerEnv TH.Q a)
  deriving (Functor, Applicative, Monad, MonadReader ScannerEnv, MonadFail)

instance TH.Quote Scan where
  newName :: String -> Scan TH.Name
  newName name = Scan . lift $ TH.newName name

getObjectMap :: Scan ObjectMap
getObjectMap = asks $ \env -> env.scannerObjectMap

getObjectConvert :: String -> Scan ObjectInfo
getObjectConvert name = do
  oMap <- getObjectMap
  maybe (undefined defObject) pure (M.lookup name oMap)
 where
  defObject :: TH.Q (TH.Type, TH.Exp, TH.Exp)
  defObject = do
    resourceType <- [t|Ptr Resource|]
    pureE <- [e|pure|]
    constPureE <- [e|const pure|]
    pure (resourceType, pureE, constPureE)

scannerIO :: IO a -> Scan a
scannerIO = Scan . lift . TH.runIO

runScanner :: Scan a -> ScannerEnv -> TH.Q a
runScanner (Scan act) = runReaderT act
