{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Actuators where

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
import           Data.Aeson as JSON
import           Data.AesonBson
import           Servant
import           Keycloak as KC hiding (info, warn, debug, err, Scope) 
import           Orion as O hiding (info, warn, debug, err)
import           System.Log.Logger
import           Paths_Waziup_Servant
import           Database.MongoDB as DB hiding (value)
import           MQTT hiding (info, warn, debug, err, Scope) 


getActuators :: Maybe Token -> DeviceId -> Waziup [Actuator]
getActuators tok did = do
  info "Get actuators"
  withKCId did $ \(keyId, device) -> do
    debug "Check permissions"
    runKeycloak $ checkPermission keyId (pack $ show DevicesView) tok
    debug "Permission granted, returning actuators"
    return $ devActuators device

postActuator :: Maybe Token -> DeviceId -> Actuator -> Waziup NoContent
postActuator tok did actuator = do
  info $ "Post actuator: " ++ (show actuator)
  withKCId did $ \(keyId, _) -> do
    debug "Check permissions"
    runKeycloak $ checkPermission keyId (pack $ show DevicesUpdate) tok
    debug "Permission granted, creating actuator"
    let att = getAttFromActuator actuator
    runOrion $ O.postAttribute (toEntityId did) att 
    return NoContent
 
getActuator :: Maybe Token -> DeviceId -> ActuatorId -> Waziup Actuator
getActuator tok did aid = do
  info "Get actuator"
  withKCId did $ \(keyId, device) -> do
     debug "Check permissions"
     runKeycloak $ checkPermission keyId (pack $ show DevicesView) tok
     debug "Permission granted, returning actuator"
     case L.find (\s -> actId s == aid) (devActuators device) of
       Just act -> return act
       Nothing -> do 
         warn "Actuator not found"
         throwError err404 {errBody = "Actuator not found"}

deleteActuator :: Maybe Token -> DeviceId -> ActuatorId -> Waziup NoContent
deleteActuator tok did aid = do
  info "Delete actuator"
  withKCId did $ \(keyId, _) -> do
    debug "Check permissions"
    runKeycloak $ checkPermission keyId (pack $ show DevicesUpdate) tok
    debug "Permission granted, deleting actuator"
    runOrion $ O.deleteAttribute (toEntityId did) (toAttributeId aid)
    return NoContent

putActuatorName :: Maybe Token -> DeviceId -> ActuatorId -> ActuatorName -> Waziup NoContent
putActuatorName mtok did aid name = do
  info $ "Put actuator name: " ++ (show name)
  updateActuatorField mtok did aid $ \act -> do 
    runOrion $ O.postAttribute (toEntityId did) $ getAttFromActuator (act {actName = Just name})

putActActuatorKind :: Maybe Token -> DeviceId -> ActuatorId -> ActuatorKindId -> Waziup NoContent
putActActuatorKind mtok did aid ak = do
  info $ "Put actuator kind: " ++ (show ak)
  updateActuatorField mtok did aid $ \act -> do 
    runOrion $ O.postAttribute (toEntityId did) $ getAttFromActuator (act {actActuatorKind = Just ak})

putActuatorValueType :: Maybe Token -> DeviceId -> ActuatorId -> ActuatorValueTypeId -> Waziup NoContent
putActuatorValueType mtok did aid av = do
  info $ "Put actuator quantity kind: " ++ (show av)
  updateActuatorField mtok did aid $ \act -> do 
    runOrion $ O.postAttribute (toEntityId did) $ getAttFromActuator (act {actActuatorValueType = Just av})

putActuatorValue :: Maybe Token -> DeviceId -> ActuatorId -> JSON.Value -> Waziup NoContent
putActuatorValue mtok did aid actVal = do
  info $ "Put actuator value: " ++ (show actVal)
  withKCId did $ \(keyId, device) -> do
     debug "Check permissions"
     runKeycloak $ checkPermission keyId (pack $ show DevicesUpdate) mtok
     debug "Permission granted, returning actuator"
     case L.find (\s -> actId s == aid) (devActuators device) of
       Just act -> do
         runOrion $ O.postAttribute (toEntityId did) $ getAttFromActuator (act {actValue = Just actVal})
         if (devVisibility device == Just Public) then liftIO $ publishActuatorValue did aid actVal else return ()
         return NoContent
       Nothing -> do 
         warn "Actuator not found"
         throwError err404 {errBody = "Actuator not found"}

updateActuatorField :: Maybe Token -> DeviceId -> ActuatorId -> (Actuator -> Waziup ()) -> Waziup NoContent
updateActuatorField mtok did aid w = do
  withKCId did $ \(keyId, device) -> do
    debug "Check permissions"
    runKeycloak $ checkPermission keyId (pack $ show DevicesUpdate) mtok
    debug "Permission granted, updating actuator"
    case L.find (\s -> (actId s) == aid) (devActuators device) of
      Just act -> do
        w act
        return NoContent
      Nothing -> do 
        warn "actuator not found"
        throwError err404 {errBody = "Actuator not found"}

toAttributeId :: ActuatorId -> AttributeId
toAttributeId (ActuatorId sid) = AttributeId sid


-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Sensors" s
info  s = liftIO $ infoM    "Sensors" s
warn  s = liftIO $ warningM "Sensors" s
err   s = liftIO $ errorM   "Sensors" s

