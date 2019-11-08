{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Devices where

import           Waziup.Types as W
import           Waziup.Utils as U
import           Waziup.Auth hiding (info, warn, debug, err) 
import           Control.Monad.IO.Class
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
import           Keycloak as KC hiding (info, warn, debug, err, createResource, updateResource, deleteResource) 
import           Orion as O hiding (info, warn, debug, err)
import           System.Log.Logger
import           Database.MongoDB as DB hiding (value, Limit, Array, lookup, Value, Null, (!?))
import           Data.AesonBson

-- | Get devices, given a query, limits and offsets
getDevices :: Maybe Token -> Maybe DevicesQuery -> Maybe Limit -> Maybe Offset -> Waziup [Device]
getDevices tok mq mlimit moffset = do
  info "Get devices"
  devices <- getAllDevices mq
  info "Got devices Orion"
  ps <- getPerms tok (getPermReq Nothing [DevicesView])
  -- filter devices not permitted
  let devices2 = filter (\d -> isPermittedResource DevicesView (PermDeviceId $ devId d) ps) devices
  -- remove offset devices
  let devices3 = maybe' devices2 L.drop moffset
  -- cut at the limit
  let devices4 = maybe' devices3 L.take mlimit
  return devices4

getAllDevices :: Maybe DevicesQuery -> Waziup [Device]
getAllDevices mq = do
  entities <- liftOrion $ O.getEntities mq devTyp
  return $  map getDeviceFromEntity entities

-- | get a sigle device
getDevice :: Maybe Token -> DeviceId -> Waziup Device
getDevice tok did = do
  info "Get device"
  device <- getDeviceFromEntity <$> (liftOrion $ O.getEntity (EntityId $ unDeviceId did) devTyp)
  debug "Check permissions"
  checkPermResource tok DevicesView (PermDeviceId did)
  debug "Permission granted, returning device"
  return device

postDevice :: Maybe Token -> Device -> Waziup NoContent
postDevice tok d = do
  info $ "Post device: " ++ (show d)
  debug "Check permissions"
  --liftKeycloak tok $ checkPermission (ResourceId "Devices") (fromScope DevicesCreate)
  debug "Create entity"
  let username = case tok of
       Just t -> getUsername t
       Nothing -> "guest"
  debug $ "Owner: " <> (show username)
  let entity = getEntityFromDevice (d {devOwner = Just username})
  void $ liftOrion $ O.postEntity entity
  void $ createResource tok (PermDeviceId $ devId d) (devVisibility d) (Just username)
  return NoContent

deleteDevice :: Maybe Token -> DeviceId -> Waziup NoContent
deleteDevice tok did = do
  info "Delete device"
  debug "Check permissions"
  checkPermResource tok DevicesDelete (PermDeviceId did)
  debug "Delete Keycloak resource"
  deleteResource tok (PermDeviceId did)
  debug "Delete Orion resource"
  liftOrion $ O.deleteEntity (toEntityId did) devTyp
  debug "Delete Mongo resources"
  runMongo $ deleteDeviceDatapoints did
  return NoContent

putDeviceLocation :: Maybe Token -> DeviceId -> Location -> Waziup NoContent
putDeviceLocation mtok did loc = do
  info $ "Put device location: " ++ (show loc)
  debug "Check permissions"
  checkPermResource mtok DevicesUpdate (PermDeviceId did)
  debug "Update Orion resource"
  liftOrion $ O.postAttribute (toEntityId did) devTyp (getLocationAttr loc)
  return NoContent

putDeviceName :: Maybe Token -> DeviceId -> DeviceName -> Waziup NoContent
putDeviceName mtok did name = do
  info $ "Put device name: " ++ (show name)
  debug "Check permissions"
  checkPermResource mtok DevicesUpdate (PermDeviceId did)
  debug "Update Orion resource"
  liftOrion $ O.postTextAttributeOrion (toEntityId did) devTyp (AttributeId "name") name
  return NoContent

putDeviceGatewayId :: Maybe Token -> DeviceId -> GatewayId -> Waziup NoContent
putDeviceGatewayId mtok did (GatewayId gid) = do
  info $ "Put device gateway ID: " ++ (show gid)
  debug "Check permissions"
  checkPermResource mtok DevicesUpdate (PermDeviceId did)
  debug "Update Orion resource"
  liftOrion $ O.postTextAttributeOrion (toEntityId did) devTyp (AttributeId "gateway_id") gid
  return NoContent

putDeviceVisibility :: Maybe Token -> DeviceId -> Visibility -> Waziup NoContent
putDeviceVisibility mtok did vis = do
  info $ "Put device visibility: " ++ (show vis)
  debug "Check permissions"
  checkPermResource mtok DevicesUpdate (PermDeviceId did)
  debug "Update Orion resource"
  liftOrion $ O.postTextAttributeOrion (toEntityId did) devTyp (AttributeId "visibility") (fromVisibility vis)
  --update visibility in KEYCLOAK
  void $ updateResource mtok (PermDeviceId did) (Just vis) Nothing
  return NoContent

putDeviceDeployed :: Maybe Token -> DeviceId -> Bool -> Waziup NoContent
putDeviceDeployed mtok did dep = do
  info $ "Put device deployed: " ++ (show dep)
  debug "Check permissions"
  checkPermResource mtok DevicesUpdate (PermDeviceId did)
  debug "Update Orion resource"
  liftOrion $ O.postAttribute (toEntityId did) devTyp (AttributeId "deployed", O.Attribute "Bool" (Just $ toJSON dep) M.empty)
  return NoContent

-- Change the owner of a device. The device will also automatically be passed as private.
putDeviceOwner :: Maybe Token -> DeviceId -> KC.Username -> Waziup NoContent
putDeviceOwner tok did owner = do
  info "Put device owner"
  checkPermResource tok DevicesUpdate (PermDeviceId did)
  d <- getDevice tok did
  debug "Update Orion resource"
  liftOrion $ O.postAttribute (toEntityId did) devTyp (AttributeId "owner", O.Attribute "String" (Just $ toJSON owner) M.empty)
  info "Replace Keycloak resource"
  deleteResource tok (PermDeviceId did)
  void $ createResource tok (PermDeviceId did) (devVisibility d) (Just owner)
  return NoContent

-- * From Orion to Waziup types

--Orion entity type for devices
devTyp :: Maybe Text
devTyp = Just "Device"

getDeviceOrion :: DeviceId -> Waziup Device
getDeviceOrion did = getDeviceFromEntity <$> (liftOrion $ O.getEntity (EntityId $ unDeviceId did) devTyp)

getDeviceFromEntity :: O.Entity -> Device
getDeviceFromEntity (O.Entity (EntityId eId) _ attrs) = 
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
           devActuators    = Just $ mapMaybe getActuatorFromAttribute (toList attrs)}

getLocation :: Map O.AttributeId O.Attribute -> Maybe Location
getLocation attrs = do 
    (O.Attribute _ mval _) <- attrs !? "location"
    (Object o) <- mval
    (Array a) <- lookup "coordinates" $ H.toList o
    let [Number lon, Number lat] = V.toList a
    return $ Location (Latitude $ toRealFloat lat) (Longitude $ toRealFloat lon)

getSensorFromAttribute :: (O.AttributeId, O.Attribute) -> Maybe Sensor
getSensorFromAttribute (AttributeId name, O.Attribute aType value mets) =
  if (aType == "Sensor") 
    then Just $ Sensor { senId            = SensorId name,
                         senName          = fromSimpleMetadata (MetadataId "name") mets,
                         senQuantityKind  = QuantityKindId <$> fromSimpleMetadata (MetadataId "quantity_kind") mets,
                         senSensorKind    = SensorKindId   <$> fromSimpleMetadata (MetadataId "sensing_device") mets,
                         senUnit          = UnitId         <$> fromSimpleMetadata (MetadataId "unit") mets,
                         senValue         = getSensorValue value mets cal,
                         senCalib         = cal}
    else Nothing where
      cal = getSensorCalib mets


getActuatorFromAttribute :: (AttributeId, O.Attribute) -> Maybe Actuator
getActuatorFromAttribute (AttributeId name, O.Attribute aType value mets) =
  if (aType == "Actuator") 
    then Just $ Actuator { actId                = ActuatorId name,
                           actName              = fromSimpleMetadata (MetadataId "name") mets,
                           actActuatorKind      = ActuatorKindId <$> fromSimpleMetadata (MetadataId "actuator_kind") mets,
                           actActuatorValueType = join $ readValueType  <$> fromSimpleMetadata (MetadataId "actuator_value_type") mets,
                           actValue             = value}
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
   value <- mval
   case fromJSON value of
     Success a -> Just a
     JSON.Error _ -> Nothing

getCalibratedValue :: Value -> Maybe Calib -> Value
getCalibratedValue (Number value) (Just cal) = Number $ fromFloatDigits $ getCalibratedValue' (toRealFloat value) cal
getCalibratedValue a _ = a

getCalibratedValue' :: Double -> Calib -> Double
getCalibratedValue' value (Linear (CalibLinear enabled (CalibValue maxSen maxReal) (CalibValue minSen minReal))) = 
  if enabled 
    then (value - minSen) * (maxReal - minReal) / (maxSen - minSen) + minReal 
    else value
getCalibratedValue' _ (W.Function _) = error "Function calibration not yet supported" 

isNull :: Value -> Bool
isNull Null = True
isNull _    = False


-- * From Waziup to Orion types

getEntityFromDevice :: Device -> O.Entity
getEntityFromDevice (Device (DeviceId sid) sgid sname sloc sdom svis sensors acts sown sdep _ _) = 
  O.Entity (EntityId sid) (fromJust devTyp) $ fromList $ catMaybes [getSimpleAttr (AttributeId "name")        <$> sname,
                                                           getSimpleAttr (AttributeId "gateway_id")  <$> (unGatewayId <$> sgid),
                                                           getSimpleAttr (AttributeId "owner")       <$> sown,
                                                           getSimpleAttr (AttributeId "domain")      <$> sdom,
                                                           getSimpleAttr (AttributeId "visibility")  <$> (fromVisibility <$> svis),
                                                           Just (AttributeId "deployed", O.Attribute "Bool" (Just $ toJSON sdep) M.empty),
                                                           getLocationAttr               <$> sloc] <>
                                                           map getAttFromSensor (maybeToList' sensors) <>
                                                           map getAttFromActuator (maybeToList' acts)

getLocationAttr :: Location -> (O.AttributeId, O.Attribute)
getLocationAttr (Location (Latitude lat) (Longitude lon)) = (AttributeId "location", O.Attribute "geo:json" (Just $ object ["type" .= ("Point" :: Text), "coordinates" .= [lon, lat]]) M.empty)

getAttFromSensor :: Sensor -> (O.AttributeId, O.Attribute)
getAttFromSensor (Sensor (SensorId sid) name sd qk u lv cal) = 
  (AttributeId sid, O.Attribute "Sensor"
                     (senValValue <$> lv)
                     (fromList $ catMaybes [getTextMetadata (MetadataId "name")           <$> name,
                                            getTextMetadata (MetadataId "quantity_kind")  <$> unQuantityKindId <$> qk,
                                            getTextMetadata (MetadataId "sensing_device") <$> unSensorKindId <$> sd,
                                            getTextMetadata (MetadataId "unit")           <$> unUnitId <$> u,
                                            getTimeMetadata (MetadataId "timestamp")      <$> (join $ senValTimestamp <$> lv),
                                            if (isJust cal) then Just $ (MetadataId "calib", Metadata  (Just "Calib") (toJSON <$> cal)) else Nothing]))

getAttFromActuator :: Actuator -> (O.AttributeId, O.Attribute)
getAttFromActuator (Actuator (ActuatorId aid) name ak avt av) = 
  (AttributeId aid, O.Attribute "Actuator"
                     av
                     (fromList $ catMaybes [getTextMetadata (MetadataId "name")           <$> name,
                                            getTextMetadata (MetadataId "actuator_kind")  <$> unActuatorKindId <$> ak,
                                            getTextMetadata (MetadataId "actuator_value_type") <$> convertString.show <$> avt]))


toEntityId :: DeviceId -> EntityId
toEntityId (DeviceId did) = EntityId did


-- * Mongo datapoints

postDatapoint :: Datapoint -> Action IO ()
postDatapoint d = do
  debug "Post datapoint to Mongo"
  let ob = case toJSON d of
       JSON.Object o -> o
       _ -> error "Wrong object format"
  void $ insert "waziup_history" (bsonify ob)

postDatapointFromSensor :: DeviceId -> Sensor -> Action IO ()
postDatapointFromSensor did (Sensor sid _ _ _ _ (Just (SensorValue v t rt)) _) = postDatapoint $ Datapoint did sid v t rt
postDatapointFromSensor _ _ = return ()

postDatapointsFromDevice :: Device -> Action IO ()
postDatapointsFromDevice (Device did _ _ _ _ _ ss _ _ _ _ _) = void $ forM (maybeToList' ss) $ postDatapointFromSensor did

deleteSensorDatapoints :: DeviceId -> SensorId -> Action IO ()
deleteSensorDatapoints (DeviceId did) (SensorId sid) = do
  debug "delete datapoints from Mongo"
  void $ delete $ select ["device_id" =: did, "sensor_id" := val sid] "waziup_history"

deleteDeviceDatapoints :: DeviceId -> Action IO ()
deleteDeviceDatapoints (DeviceId did) = do
  debug "delete datapoints from Mongo"
  void $ delete $ select ["device_id" =: did] "waziup_history"


-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Devices" s
info  s = liftIO $ infoM    "Devices" s
warn  s = liftIO $ warningM "Devices" s
err   s = liftIO $ errorM   "Devices" s

