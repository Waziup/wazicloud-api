{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Devices where

import           Waziup.Types as W
import           Waziup.Utils
import           Waziup.Auth hiding (info, warn, debug, err) 
import           Control.Monad.Except (throwError)
import           Control.Monad.IO.Class
import           Control.Monad.Catch as C
import           Control.Monad
import           Data.Maybe
import           Data.Map as M hiding (map, mapMaybe, filter, lookup, insert, delete)
import           Data.Text hiding (map, filter, foldl, any)
import           Data.String.Conversions
import qualified Data.List as L
import qualified Data.Vector as V
import           Data.Scientific
import qualified Data.HashMap.Strict as H
import           Data.Aeson as JSON hiding (Options)
import           Data.Time.ISO8601
import           Servant
import           Keycloak as KC hiding (info, warn, debug, err) 
import           Orion as O hiding (info, warn, debug, err)
import           System.Log.Logger
import           Paths_Waziup_Servant
import           Database.MongoDB as DB hiding (value, Limit, Array, lookup, Value, Null, (!?))
import           Data.AesonBson

--Orion entity type for devices
devTyp :: Maybe Text
devTyp = Just "Device"

-- | Get devices, given a query, limits and offsets
getDevices :: Maybe Token -> Maybe DevicesQuery -> Maybe Limit -> Maybe Offset -> Waziup [Device]
getDevices tok mq mlimit moffset = do
  info "Get devices"
  entities <- liftOrion $ O.getEntities mq devTyp
  let devices = map getDeviceFromEntity entities
  ps <- getPermsDevices tok
  -- filter devices not permitted
  let devices2 = filter (checkPermDevice DevicesView ps . devId) devices
  -- remove ofseted devices
  let devices3 = maybe' devices2 L.drop moffset
  -- cut at the limit
  let devices4 = maybe' devices3 L.take mlimit
  return devices4

checkPermDevice :: W.Scope -> [Perm] -> DeviceId -> Bool
checkPermDevice scope perms dev = any (\p -> (permResource p) == (unDeviceId $ dev) && scope `elem` (permScopes p)) perms
 
-- | get a sigle device
getDevice :: Maybe Token -> DeviceId -> Waziup Device
getDevice tok did = do
  info "Get device"
  withKCId did $ \(keyId, device) -> do
    debug "Check permissions"
    liftKeycloak tok $ checkPermission keyId (fromScope DevicesView)
    debug "Permission granted, returning device"
    return device

postDevice :: Maybe Token -> Device -> Waziup NoContent
postDevice tok d = do
  info $ "Post device: " ++ (show d)
  debug "Check permissions"
  liftKeycloak tok $ checkPermission (ResourceId "Devices") (fromScope DevicesCreate)
  debug "Create entity"
  let username = case tok of
       Just t -> getUsername t
       Nothing -> "guest"
  debug $ "Owner: " <> (show username)
  let entity = getEntityFromDevice (d {devOwner = Just username})
  res2 <- C.try $ liftOrion $ O.postEntity entity
  let scopes = [DevicesView, DevicesUpdate, DevicesDelete, DevicesDataCreate, DevicesDataView]
  let attrs = if (isJust $ devVisibility d) then [KC.Attribute "visibility" [fromVisibility $ fromJust $ devVisibility d]] else []
  let did = unDeviceId $ devId d
  case res2 of
    Right _ -> do 
      keyRes <- C.try $ createResource' tok Nothing did "device" scopes attrs                                 
      case keyRes of
        Right (ResourceId resId) -> do
          liftOrion $ O.postTextAttributeOrion (EntityId did) devTyp (AttributeId "keycloak_id") resId
          return NoContent
        Left e -> do
          err $ "Keycloak error: " ++ (show e) ++ " deleting device"
          (_ :: Either ServantErr ()) <- C.try $ liftOrion $ O.deleteEntity (EntityId did) devTyp
          throwError e
    Left (err :: ServantErr)  -> do
      warn "Orion error"
      throwError err500 {errBody = "Not a Waziup device"}
 
deleteDevice :: Maybe Token -> DeviceId -> Waziup NoContent
deleteDevice tok did = do
  info "Delete device"
  withKCId did $ \(keyId, _) -> do
    debug "Check permissions"
    liftKeycloak tok $ checkPermission keyId (fromScope DevicesDelete)
    debug "Delete Keycloak resource"
    liftKeycloak tok $ deleteResource keyId
    debug "Delete Orion resource"
    liftOrion $ O.deleteEntity (toEntityId did) devTyp
    debug "Delete Mongo resources"
    runMongo $ deleteDeviceDatapoints did
    return NoContent

putDeviceLocation :: Maybe Token -> DeviceId -> Location -> Waziup NoContent
putDeviceLocation mtok did loc = do
  info $ "Put device location: " ++ (show loc)
  withKCId did $ \(keyId, _) -> do
    debug "Check permissions"
    liftKeycloak mtok $ checkPermission keyId (fromScope DevicesUpdate)

    debug "Update Orion resource"
    let att = getLocationAttr loc
    liftOrion $ O.postAttribute (toEntityId did) devTyp att
  return NoContent

putDeviceName :: Maybe Token -> DeviceId -> DeviceName -> Waziup NoContent
putDeviceName mtok did name = do
  info $ "Put device name: " ++ (show name)
  withKCId did $ \(keyId, _) -> do
    debug "Check permissions"
    liftKeycloak mtok $ checkPermission keyId (fromScope DevicesUpdate)
    debug "Update Orion resource"
    liftOrion $ O.postTextAttributeOrion (toEntityId did) devTyp (AttributeId "name") name
  return NoContent

putDeviceGatewayId :: Maybe Token -> DeviceId -> GatewayId -> Waziup NoContent
putDeviceGatewayId mtok did (GatewayId gid) = do
  info $ "Put device gateway ID: " ++ (show gid)
  withKCId did $ \(keyId, _) -> do
    debug "Check permissions"
    liftKeycloak mtok $ checkPermission keyId (fromScope DevicesUpdate)
    debug "Update Orion resource"
    liftOrion $ O.postTextAttributeOrion (toEntityId did) devTyp (AttributeId "gateway_id") gid
  return NoContent

putDeviceVisibility :: Maybe Token -> DeviceId -> Visibility -> Waziup NoContent
putDeviceVisibility mtok did vis = do
  info $ "Put device visibility: " ++ (show vis)
  withKCId did $ \(keyId, _) -> do
    debug "Check permissions"
    liftKeycloak mtok $ checkPermission keyId (fromScope DevicesUpdate)
    debug "Update Orion resource"
    liftOrion $ O.postTextAttributeOrion (toEntityId did) devTyp (AttributeId "visibility") (fromVisibility vis)
    --TODO: update visibility in KEYCLOAK
  return NoContent

putDeviceDeployed :: Maybe Token -> DeviceId -> Bool -> Waziup NoContent
putDeviceDeployed mtok did dep = do
  info $ "Put device deployed: " ++ (show dep)
  withKCId did $ \(keyId, _) -> do
    debug "Check permissions"
    liftKeycloak mtok $ checkPermission keyId (fromScope DevicesUpdate)
    debug "Update Orion resource"
    liftOrion $ O.postAttribute (toEntityId did) devTyp (AttributeId "deployed", O.Attribute "Bool" (Just $ toJSON dep) M.empty)
  return NoContent
-- * From Orion to Waziup types

getDeviceFromEntity :: O.Entity -> Device
getDeviceFromEntity (O.Entity (EntityId eId) eType attrs) = 
  Device { devId           = DeviceId eId,
           devGatewayId    = GatewayId <$> O.fromSimpleAttribute (AttributeId "gateway_id") attrs,
           devName         = fromSimpleAttribute (AttributeId "name") attrs,
           devOwner        = fromSimpleAttribute (AttributeId "owner") attrs,
           devLocation     = getLocation attrs,
           devDomain       = fromSimpleAttribute (AttributeId "domain") attrs,
           devVisibility   = fromSimpleAttribute (AttributeId "visibility") attrs >>= toVisibility,
           devDeployed     = fromBoolAttribute (AttributeId "deployed") attrs,
           devDateCreated  = fromSimpleAttribute (AttributeId "dateCreated") attrs >>= parseISO8601.unpack,
           devDateModified = fromSimpleAttribute (AttributeId "dateModified") attrs >>= parseISO8601.unpack,
           devSensors      = Just $ mapMaybe getSensorFromAttribute (toList attrs),
           devActuators    = Just $ mapMaybe getActuatorFromAttribute (toList attrs),
           devKeycloakId   = ResourceId <$> O.fromSimpleAttribute (AttributeId "keycloak_id") attrs}

getLocation :: Map O.AttributeId O.Attribute -> Maybe Location
getLocation attrs = do 
    (O.Attribute _ mval _) <- attrs !? "location"
    (Object o) <- mval
    (Array a) <- lookup "coordinates" $ H.toList o
    let [Number lon, Number lat] = V.toList a
    return $ Location (Latitude $ toRealFloat lat) (Longitude $ toRealFloat lon)

getSensorFromAttribute :: (O.AttributeId, O.Attribute) -> Maybe Sensor
getSensorFromAttribute (AttributeId name, O.Attribute aType val mets) =
  if (aType == "Sensor") 
    then Just $ Sensor { senId            = SensorId name,
                         senName          = fromSimpleMetadata (MetadataId "name") mets,
                         senQuantityKind  = QuantityKindId <$> fromSimpleMetadata (MetadataId "quantity_kind") mets,
                         senSensorKind    = SensorKindId   <$> fromSimpleMetadata (MetadataId "sensing_device") mets,
                         senUnit          = UnitId         <$> fromSimpleMetadata (MetadataId "unit") mets,
                         senValue         = getSensorValue val mets cal,
                         senCalib         = cal}
    else Nothing where
      cal = getSensorCalib mets


getActuatorFromAttribute :: (AttributeId, O.Attribute) -> Maybe Actuator
getActuatorFromAttribute (AttributeId name, O.Attribute aType val mets) =
  if (aType == "Actuator") 
    then Just $ Actuator { actId                = ActuatorId name,
                           actName              = fromSimpleMetadata (MetadataId "name") mets,
                           actActuatorKind      = ActuatorKindId <$> fromSimpleMetadata (MetadataId "actuator_kind") mets,
                           actActuatorValueType = join $ readValueType  <$> fromSimpleMetadata (MetadataId "actuator_value_type") mets,
                           actValue             = val}
    else Nothing

getSensorValue :: Maybe Value -> Map O.MetadataId O.Metadata -> Maybe Calib -> Maybe SensorValue
getSensorValue mval mets cal = do
   value <- mval
   guard $ not $ isNull value
   let valueCalib = getCalibratedValue value cal
   return $ SensorValue valueCalib 
                        (O.fromSimpleMetadata (MetadataId "timestamp")    mets >>= parseISO8601.unpack)
                        (O.fromSimpleMetadata (MetadataId "dateModified") mets >>= parseISO8601.unpack)

getSensorCalib :: Map O.MetadataId O.Metadata -> Maybe Calib
getSensorCalib mets = do
   (Metadata _ mval) <- mets !? "calib"
   val <- mval
   case fromJSON val of
     Success a -> Just a
     JSON.Error _ -> Nothing

getCalibratedValue :: Value -> Maybe Calib -> Value
getCalibratedValue (Number val) (Just cal) = Number $ fromFloatDigits $ getCalibratedValue' (toRealFloat val) cal
getCalibratedValue a _ = a

getCalibratedValue' :: Double -> Calib -> Double
getCalibratedValue' val (Linear (CalibLinear enabled (CalibValue maxSen maxReal) (CalibValue minSen minReal))) = 
  if enabled 
    then (val - minSen) * (maxReal - minReal) / (maxSen - minSen) + minReal 
    else val

isNull :: Value -> Bool
isNull Null = True
isNull _    = False


-- * From Waziup to Orion types

getEntityFromDevice :: Device -> O.Entity
getEntityFromDevice (Device (DeviceId sid) sgid sname sloc sdom svis sensors acts sown sdep _ _ skey) = 
  O.Entity (EntityId sid) (fromJust devTyp) $ fromList $ catMaybes [getSimpleAttr (AttributeId "name")        <$> sname,
                                                           getSimpleAttr (AttributeId "gateway_id")  <$> (unGatewayId <$> sgid),
                                                           getSimpleAttr (AttributeId "owner")       <$> sown,
                                                           getSimpleAttr (AttributeId "domain")      <$> sdom,
                                                           getSimpleAttr (AttributeId "keycloak_id") <$> (unResId <$> skey),
                                                           getSimpleAttr (AttributeId "visibility")  <$> (fromVisibility <$> svis),
                                                           Just (AttributeId "deployed", O.Attribute "Bool" (Just $ toJSON sdep) M.empty),
                                                           getLocationAttr               <$> sloc] <>
                                                           map getAttFromSensor (maybeToList' sensors) <>
                                                           map getAttFromActuator (maybeToList' acts)

getLocationAttr :: Location -> (O.AttributeId, O.Attribute)
getLocationAttr (Location (Latitude lat) (Longitude lon)) = (AttributeId "location", O.Attribute "geo:json" (Just $ object ["type" .= ("Point" :: Text), "coordinates" .= [lon, lat]]) M.empty)

getAttFromSensor :: Sensor -> (O.AttributeId, O.Attribute)
getAttFromSensor (Sensor (SensorId senId) name sd qk u lv cal) = 
  (AttributeId senId, O.Attribute "Sensor"
                     (senValValue <$> lv)
                     (fromList $ catMaybes [getTextMetadata (MetadataId "name")           <$> name,
                                            getTextMetadata (MetadataId "quantity_kind")  <$> unQuantityKindId <$> qk,
                                            getTextMetadata (MetadataId "sensing_device") <$> unSensorKindId <$> sd,
                                            getTextMetadata (MetadataId "unit")           <$> unUnitId <$> u,
                                            getTimeMetadata (MetadataId "timestamp")      <$> (join $ senValTimestamp <$> lv),
                                            if (isJust cal) then Just $ (MetadataId "calib", Metadata  (Just "Calib") (toJSON <$> cal)) else Nothing]))

getAttFromActuator :: Actuator -> (O.AttributeId, O.Attribute)
getAttFromActuator (Actuator (ActuatorId actId) name ak avt av) = 
  (AttributeId actId, O.Attribute "Actuator"
                     av
                     (fromList $ catMaybes [getTextMetadata (MetadataId "name")           <$> name,
                                            getTextMetadata (MetadataId "actuator_kind")  <$> unActuatorKindId <$> ak,
                                            getTextMetadata (MetadataId "actuator_value_type") <$> convertString.show <$> avt]))


withKCId :: DeviceId -> ((ResourceId, Device) -> Waziup a) -> Waziup a
withKCId (DeviceId did) f = do
  device <- getDeviceFromEntity <$> (liftOrion $ O.getEntity (EntityId did) devTyp)
  case (devKeycloakId device) of
    Just keyId -> f (keyId, device) 
    Nothing -> do
      error "Device: KC Id not present"
      throwError err500 {errBody = "Device: KC Id not present"}

toEntityId :: DeviceId -> EntityId
toEntityId (DeviceId did) = EntityId did


-- * Mongo datapoints

postDatapoint :: Datapoint -> Action IO ()
postDatapoint d = do
  debug "Post datapoint to Mongo"
  let ob = case toJSON d of
       JSON.Object o -> o
       _ -> error "Wrong object format"
  res <- insert "waziup_history" (bsonify ob)
  return ()

postDatapointFromSensor :: DeviceId -> Sensor -> Action IO ()
postDatapointFromSensor did (Sensor sid _ _ _ _ (Just (SensorValue v t rt)) _) = postDatapoint $ Datapoint did sid v t rt
postDatapointFromSensor did _ = return ()

postDatapointsFromDevice :: Device -> Action IO ()
postDatapointsFromDevice (Device did _ _ _ _ _ ss _ _ _ _ _ _) = void $ forM (maybeToList' ss) $ postDatapointFromSensor did
postDatapointsFromDevice _ = return ()

deleteSensorDatapoints :: DeviceId -> SensorId -> Action IO ()
deleteSensorDatapoints (DeviceId did) (SensorId sid) = do
  debug "delete datapoints from Mongo"
  res <- delete $ select ["device_id" =: did, "sensor_id" := val sid] "waziup_history"
  return ()

deleteDeviceDatapoints :: DeviceId -> Action IO ()
deleteDeviceDatapoints (DeviceId did) = do
  debug "delete datapoints from Mongo"
  res <- delete $ select ["device_id" =: did] "waziup_history"
  return ()


-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Devices" s
info  s = liftIO $ infoM    "Devices" s
warn  s = liftIO $ warningM "Devices" s
err   s = liftIO $ errorM   "Devices" s

