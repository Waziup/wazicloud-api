{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.SensorData where

import           Waziup.Types
import           Waziup.Utils
import           Waziup.Devices hiding (info, warn, debug, err)
import           Control.Monad.Except (throwError)
import           Control.Monad.IO.Class
import           Control.Monad.Catch as C
import           Data.Maybe
import           Data.Text hiding (map, filter, foldl, any)
import           Data.String.Conversions
import qualified Data.List as L
import           Servant
import           Keycloak as KC hiding (info, warn, debug, err, Scope) 
import           Orion as O hiding (info, warn, debug, err)
import           Mongo as M hiding (info, warn, debug, err)
import           System.Log.Logger


getDatapoints :: Maybe Token -> DeviceId -> SensorId -> Waziup [Datapoint]
getDatapoints tok did sid = do
  info "Get datapoints"
  withKCId did $ \(keyId, _) -> do
      debug "Check permissions"
      runKeycloak $ checkPermission keyId (pack $ show DevicesDataView) tok
      debug "Permission granted, returning datapoints"
      runMongo $ M.getDatapointsMongo did sid

-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "SensorData" s
info  s = liftIO $ infoM    "SensorData" s
warn  s = liftIO $ warningM "SensorData" s
err   s = liftIO $ errorM   "SensorData" s

