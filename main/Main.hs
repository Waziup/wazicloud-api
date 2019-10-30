{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Main where

import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           Network.Wai.Middleware.Cors
import           Waziup.Server
import           Waziup.Types 
import           Waziup.Config 
import           Data.String.Conversions
import           Data.Aeson hiding (Success)
import qualified Data.ByteString as BS
import           Data.Validation
import           Data.Foldable
import           Data.Maybe
import           Data.Pool
import           System.Log.Logger
import           System.Log.Formatter
import           System.Log.Handler hiding (setLevel)
import           System.Log.Handler.Simple
import           System.Log.Handler.Log4jXML
import           System.IO
import           Keycloak hiding (try)
import           Orion hiding (try)
import           Database.MongoDB as DB hiding (value)
import           Options.Applicative as Opts hiding (Success, Failure)
import           Control.Exception as C
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Concurrent
import           System.FilePath ((</>))
import           System.Environment
import           Paths_Waziup_Servant
import           MQTT
import           Web.Twitter.Conduit hiding (map)
import           Data.Map as M hiding (map)
import           Control.Concurrent.STM
import           Data.Time


main :: IO ()
main = do
  waziupInfo <- configureWaziup
  let host = waziupInfo ^. waziupConfig.serverConf.serverHost
  let port = waziupInfo ^. waziupConfig.serverConf.serverPort
  let mqttPort = waziupInfo ^. waziupConfig.serverConf.serverPortMQTT
  Main.info $ "API server starting..."
  Main.info $ convertString $ "HTTP API is running on " <> host <> "/api/v2"
  Main.info $ convertString $ "MQTT is running on port " <> (show mqttPort)
  Main.info $ convertString $ "Documentation is on " <> host <> "/docs"
  forkIO $ mqttProxy waziupInfo
  run port $ logStdout
           $ cors (const $ Just corsPolicy)
           $ waziupServer waziupInfo


corsPolicy :: CorsResourcePolicy
corsPolicy = simpleCorsResourcePolicy
           { corsRequestHeaders = ["Access-Control-Allow-Origin", "Authorization", "Content-Type"],
             corsMethods        = ["OPTIONS", "HEAD", "POST", "PUT", "GET", "DELETE"]}


-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM "Main" s
info s  = liftIO $ infoM "Main" s
warn s  = liftIO $ warningM "Main" s
err s   = liftIO $ errorM "Main" s
