{-# LANGUAGE TemplateHaskellQuotes #-}

module Graphics.Wayland.Scanner.WLS (
  ObjectInfo (..),
  Scanner (..),
  Scan,
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

newtype Scanner m a = Scanner (ReaderT ScannerEnv m a)
  deriving (Functor, Applicative, Monad, MonadReader ScannerEnv, MonadFail)

instance (Monad m) => TH.Quote (Scanner m)
type Scan = Scanner TH.Q

getObjectMap :: (Monad m) => Scanner m ObjectMap
getObjectMap = asks $ \env -> env.scannerObjectMap

getObjectConvert :: (Monad m) => String -> Scanner m ObjectInfo
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

scannerIO :: IO a -> Scanner TH.Q a
scannerIO = Scanner . lift . TH.runIO

runScanner :: Scanner m a -> ScannerEnv -> m a
runScanner (Scanner act) = runReaderT act
