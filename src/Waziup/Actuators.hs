{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Actuators where

import           Waziup.Types
import           Waziup.Utils
import           Waziup.Devices hiding (info, warn, debug, err)
import           Control.Monad.Except (throwError)
import           Control.Monad.IO.Class
import           Data.Text hiding (map, filter, foldl, any)
import qualified Data.List as L
import           Data.Aeson as JSON
import           Servant
import           Keycloak as KC hiding (info, warn, debug, err, Scope) 
import           Orion as O hiding (info, warn, debug, err)
import           System.Log.Logger
import           MQTT hiding (info, warn, debug, err) 


getActuators :: Maybe Token -> DeviceId -> Waziup [Actuator]
getActuators tok did = do
  info "Get actuators"
  withKCId did $ \(keyId, device) -> do
    debug "Check permissions"
    liftKeycloak tok $ checkPermission keyId (fromScope DevicesView)
    debug "Permission granted, returning actuators"
    return $ maybeToList' $ devActuators device

postActuator :: Maybe Token -> DeviceId -> Actuator -> Waziup NoContent
postActuator tok did actuator = do
  info $ "Post actuator: " ++ (show actuator)
  withKCId did $ \(keyId, _) -> do
    debug "Check permissions"
    liftKeycloak tok $ checkPermission keyId (fromScope DevicesUpdate)
    debug "Permission granted, creating actuator"
    let att = getAttFromActuator actuator
    liftOrion $ O.postAttribute (toEntityId did) (Just "Device") att 
    return NoContent
 
getActuator :: Maybe Token -> DeviceId -> ActuatorId -> Waziup Actuator
getActuator tok did aid = do
  info "Get actuator"
  withKCId did $ \(keyId, device) -> do
     debug "Check permissions"
     liftKeycloak tok $ checkPermission keyId (fromScope DevicesView)
     debug "Permission granted, returning actuator"
     case L.find (\s -> actId s == aid) (maybeToList' $ devActuators device) of
       Just act -> return act
       Nothing -> do 
         warn "Actuator not found"
         throwError err404 {errBody = "Actuator not found"}

deleteActuator :: Maybe Token -> DeviceId -> ActuatorId -> Waziup NoContent
deleteActuator tok did aid = do
  info "Delete actuator"
  withKCId did $ \(keyId, _) -> do
    debug "Check permissions"
    liftKeycloak tok $ checkPermission keyId (fromScope DevicesUpdate)
    debug "Permission granted, deleting actuator"
    liftOrion $ O.deleteAttribute (toEntityId did) (Just "Device") (toAttributeId aid)
    return NoContent

putActuatorName :: Maybe Token -> DeviceId -> ActuatorId -> ActuatorName -> Waziup NoContent
putActuatorName mtok did aid name = do
  info $ "Put actuator name: " ++ (show name)
  updateActuatorField mtok did aid $ \act -> do 
    liftOrion $ O.postAttribute (toEntityId did) (Just "Device") (getAttFromActuator (act {actName = Just name}))

putActActuatorKind :: Maybe Token -> DeviceId -> ActuatorId -> ActuatorKindId -> Waziup NoContent
putActActuatorKind mtok did aid ak = do
  info $ "Put actuator kind: " ++ (show ak)
  updateActuatorField mtok did aid $ \act -> do 
    liftOrion $ O.postAttribute (toEntityId did) (Just "Device") (getAttFromActuator (act {actActuatorKind = Just ak}))

putActuatorValueType :: Maybe Token -> DeviceId -> ActuatorId -> ActuatorValueTypeId -> Waziup NoContent
putActuatorValueType mtok did aid av = do
  info $ "Put actuator quantity kind: " ++ (show av)
  updateActuatorField mtok did aid $ \act -> do 
    liftOrion $ O.postAttribute (toEntityId did) (Just "Device") (getAttFromActuator (act {actActuatorValueType = Just av}))

putActuatorValue :: Maybe Token -> DeviceId -> ActuatorId -> JSON.Value -> Waziup NoContent
putActuatorValue mtok did aid actVal = do
  info $ "Put actuator value: " ++ (show actVal)
  withKCId did $ \(keyId, device) -> do
     debug "Check permissions"
     liftKeycloak mtok $ checkPermission keyId (fromScope DevicesUpdate)
     debug "Permission granted, returning actuator"
     case L.find (\s -> actId s == aid) (maybeToList' $ devActuators device) of
       Just act -> do
         liftOrion $ O.postAttribute (toEntityId did) (Just "Device") (getAttFromActuator (act {actValue = Just actVal}))
         publishActuatorValue did aid actVal
         return NoContent
       Nothing -> do 
         warn "Actuator not found"
         throwError err404 {errBody = "Actuator not found"}

updateActuatorField :: Maybe Token -> DeviceId -> ActuatorId -> (Actuator -> Waziup ()) -> Waziup NoContent
updateActuatorField mtok did aid w = do
  withKCId did $ \(keyId, device) -> do
    debug "Check permissions"
    liftKeycloak mtok $ checkPermission keyId (fromScope DevicesUpdate)
    debug "Permission granted, updating actuator"
    case L.find (\s -> (actId s) == aid) (maybeToList' $ devActuators device) of
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

