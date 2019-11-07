{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Main where

import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Network.Wai.Middleware.Cors
import           Waziup.Server
import           Waziup.Types 
import           Waziup.Config 
import           Data.String.Conversions
import           System.Log.Logger
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Concurrent
import           MQTT


main :: IO ()
main = do
  waziupInfo <- configureWaziup
  let host = waziupInfo ^. waziupConfig.serverConf.serverHost
  let port = waziupInfo ^. waziupConfig.serverConf.serverPort
  let mqttPor = waziupInfo ^. waziupConfig.serverConf.serverPortMQTT
  Main.info $ "API server starting..."
  Main.info $ convertString $ "HTTP API is running on " <> host <> "/api/v2"
  Main.info $ convertString $ "MQTT is running on port " <> (show mqttPor)
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
