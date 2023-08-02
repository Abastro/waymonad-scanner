{-# LANGUAGE TemplateHaskellQuotes #-}

module Graphics.Wayland.Scanner (
  protocolFromFile,
)
where

import Control.Monad.Trans (MonadTrans (lift))
import Foreign.Ptr (Ptr)
import System.Process (readProcess)

import Graphics.Wayland.Scanner.Dispatcher
import Graphics.Wayland.Scanner.Marshal
import Graphics.Wayland.Scanner.Types
import Graphics.Wayland.Scanner.WLS
import Graphics.Wayland.Scanner.XML
import Utility

import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as THS
import Data.Bifunctor

-- Where, and how, can you use this..
-- I guess this does not make sense because it was.. not tested. Meh

makeInterfaceGetter :: String -> Scan TH.Dec
makeInterfaceGetter iface = TH.forImpD TH.CCall TH.Safe ('&' : ifaceName) funName importType
 where
  ifaceName = iface ++ "_interface"
  funName = TH.mkName $ replaceUnder iface ++ "Interface"
  importType = [t|Ptr Interface|]

makeInterfaceDecls :: (String, Interface, Int) -> Scan [TH.Dec]
makeInterfaceDecls (interfaceName, Interface _ reqs evts, _) = do
  getterD <- makeInterfaceGetter interfaceName
  reqD <-
    if null reqs
      then pure []
      else makeDispatcher interfaceName $ second (\(Request args) -> map snd args) <$> reqs
  evtD <- traverse (\((n, Event args), i) ->
    postEventFnDec (TH.mkName $ hsVarName interfaceName ++ "Post" ++ cleanName n) (map snd args) i) $ zip evts [0 ..]
  pure $ getterD : reqD ++ concat evtD

protocolFromFile :: String -> Scan [TH.Dec]
protocolFromFile file = do
  Protocol _ ifaces <- scannerIO $ protFromFile file
  ret <- mapM makeInterfaceDecls ifaces
  Scan . lift $ generateInterface file
  pure $ concat ret

generateInterface :: String -> TH.Q ()
generateInterface file = do
  content <- TH.runIO $ readProcess "wayland-scanner" ["code", file, "/dev/stdout"] ""
  THS.addForeignSource THS.LangC content
