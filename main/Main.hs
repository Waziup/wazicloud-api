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
import           Waziup.API 
import           Waziup.Server 
import           Waziup.Config 
import           Data.String.Conversions
import           System.Log.Logger
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Concurrent
import           MQTT
import           Servant
import           Servant.Server
import           Servant.Auth.Server
import           Crypto.JOSE.JWK as Jose
import           Data.Aeson as Aeson

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
  let jsonKey = "{\"kid\":\"Lla9_pMxUpsT1Mzl3glvDELVHknz9fCo_uCrax8uvBw\",\"kty\":\"RSA\",\"alg\":\"RS256\",\"use\":\"sig\",\"n\":\"1GD_Zs4DaccHzZlWNq74MqPwy02sWNJMXcGB0bsbWtj-oX4AzZgUIniu60I3zOVLst8zc16FzRg_vRfPSxMb-oKuRinfhKZJiZepXDi27bcsUvteprHLW8LHufvngNNzHNmrGjPsxEdhb9Mouw8DRmx6m_PgIYTv4ilSkCgNs3NteCRtDZl8_iSA0fpPwA77BV8mT8RBntZ6CbjV-zxGEsQ7ly5rAmG7ADUPFhOzM7DNDVbSAzOyK7OL5W7p0KFfpX8wphvoix2c2hAjjjhCKHSqlm88DjfnXFm6ggYbY8_TC7ZwPB_Y8xQ3mM7-vEFSnmtIrPH089kw8HWnyntCPQ\",\"e\":\"AQAB\"}"
  let myKey = case decode jsonKey of
       Nothing -> error "not decoded"
       Just e -> e

  let jwtCfg = defaultJWTSettings myKey
      cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
      api = Proxy :: Proxy API
      context = Proxy :: Proxy '[CookieSettings, JWTSettings]
      server' = hoistServerWithContext api context (getHandler waziupInfo) server
  Main.info $ show myKey
  run port $ logStdout
           $ cors (const $ Just corsPolicy)
           $ serveWithContext api cfg server'

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
