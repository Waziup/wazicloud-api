{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Actuators where

import           Waziup.Types
import           Waziup.Utils
import           Waziup.Devices hiding (info, warn, debug, err)
import           Waziup.Auth hiding (info, warn, debug, err)
import           Control.Monad.Except (throwError)
import           Control.Monad.IO.Class
import           Control.Monad
import           Control.Lens
import qualified Data.List as L
import           Data.Aeson as JSON
import           Servant
import           Keycloak as KC hiding (Scope) 
import           Orion as O hiding (info, warn, debug, err)
import           System.Log.Logger
import           MQTT hiding (info, warn, debug, err) 


getActuators :: AuthUser -> DeviceId -> Waziup [Actuator]
getActuators tok did = do
  info "Get actuators"
  device <- getDeviceOrion did
  debug "Check permissions"
  checkPermResource tok DevicesView (PermDevice device)
  debug "Permission granted, returning actuators"
  return $ maybeToList' $ devActuators device

postActuator :: AuthUser -> DeviceId -> Actuator -> Waziup NoContent
postActuator tok did actuator = do
  info $ "Post actuator: " ++ (show actuator)
  d <- getDeviceOrion did
  debug "Check permissions"
  checkPermResource tok DevicesUpdate (PermDevice d)
  debug "Permission granted, creating actuator"
  let att = getAttFromActuator actuator
  liftOrion $ O.postAttribute (toEntityId did) devTyp att 
  return NoContent
 
getActuator :: AuthUser -> DeviceId -> ActuatorId -> Waziup Actuator
getActuator tok did aid = do
  info "Get actuator"
  d <- getDeviceOrion did
  debug "Check permissions"
  checkPermResource tok DevicesView (PermDevice d)
  debug "Permission granted, returning actuator"
  device <- getDeviceOrion did
  case L.find (\s -> actId s == aid) (maybeToList' $ devActuators device) of
    Just act -> return act
    Nothing -> do 
      warn "Actuator not found"
      throwError err404 {errBody = "Actuator not found"}

deleteActuator :: AuthUser -> DeviceId -> ActuatorId -> Waziup NoContent
deleteActuator tok did aid = do
  info "Delete actuator"
  d <- getDeviceOrion did
  debug "Check permissions"
  checkPermResource tok DevicesUpdate (PermDevice d)
  debug "Permission granted, deleting actuator"
  liftOrion $ O.deleteAttribute (toEntityId did) devTyp (toAttributeId aid)
  return NoContent

putActuatorName :: AuthUser -> DeviceId -> ActuatorId -> ActuatorName -> Waziup NoContent
putActuatorName mtok did aid name = do
  info $ "Put actuator name: " ++ (show name)
  updateActuatorField mtok did aid $ \act -> do 
    liftOrion $ O.postAttribute (toEntityId did) devTyp (getAttFromActuator (act {actName = Just name}))

putActActuatorKind :: AuthUser -> DeviceId -> ActuatorId -> ActuatorKindId -> Waziup NoContent
putActActuatorKind mtok did aid ak = do
  info $ "Put actuator kind: " ++ (show ak)
  updateActuatorField mtok did aid $ \act -> do 
    liftOrion $ O.postAttribute (toEntityId did) devTyp (getAttFromActuator (act {actActuatorKind = Just ak}))

putActuatorValueType :: AuthUser -> DeviceId -> ActuatorId -> ActuatorValueTypeId -> Waziup NoContent
putActuatorValueType mtok did aid av = do
  info $ "Put actuator quantity kind: " ++ (show av)
  updateActuatorField mtok did aid $ \act -> do 
    liftOrion $ O.postAttribute (toEntityId did) devTyp (getAttFromActuator (act {actActuatorValueType = Just av}))

putActuatorValue :: AuthUser -> DeviceId -> ActuatorId -> JSON.Value -> Waziup NoContent
putActuatorValue mtok did aid actVal = do
  info $ "Put actuator value: " ++ (show actVal)
  device <- getDeviceOrion did
  debug "Check permissions"
  checkPermResource mtok DevicesUpdate (PermDevice device)
  debug "Permission granted, returning actuator"
  case L.find (\s -> actId s == aid) (maybeToList' $ devActuators device) of
    Just act -> do
      liftOrion $ O.postAttribute (toEntityId did) devTyp (getAttFromActuator (act {actValue = Just actVal}))
      mqttAct <- view $ waziupConfig.serverConf.mqttActivated
      when mqttAct $ publishActuatorValue did aid actVal
      return NoContent
    Nothing -> do 
      warn "Actuator not found"
      throwError err404 {errBody = "Actuator not found"}

updateActuatorField :: AuthUser -> DeviceId -> ActuatorId -> (Actuator -> Waziup ()) -> Waziup NoContent
updateActuatorField mtok did aid w = do
  debug "Check permissions"
  device <- getDeviceOrion did
  checkPermResource mtok DevicesUpdate (PermDevice device)
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
debug s = liftIO $ debugM   "Actuators" s
info  s = liftIO $ infoM    "Actuators" s
warn  s = liftIO $ warningM "Actuators" s
err   s = liftIO $ errorM   "Actuators" s

