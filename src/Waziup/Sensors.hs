{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Sensors where

import           Waziup.Types
import           Waziup.Utils
import           Control.Monad.Except (throwError)
import           Control.Monad.IO.Class
import           Control.Monad.Catch as C
import           Control.Monad
import           Data.Maybe
import           Data.Text hiding (map, filter, foldl, any)
import           Data.String.Conversions
import qualified Data.List as L
import qualified Data.Vector as V
import           Data.Scientific
import qualified Data.HashMap.Strict as H
import           Data.Aeson as JSON hiding (Options)
import           Data.Time.ISO8601
import           Servant
import           Keycloak as KC hiding (info, warn, debug, err, Scope) 
import           Orion as O hiding (info, warn, debug, err)
import           System.Log.Logger
import           Paths_Waziup_Servant

getPerms :: Maybe Token -> Waziup [Perm]
getPerms tok = do
  info "Get Permissions"
  let allScopes = [SensorsUpdate,
                   SensorsView,
                   SensorsDelete,
                   SensorsDataCreate,
                   SensorsDataView]
  ps <- runKeycloak $ getAllPermissions (map (convertString.show) allScopes) tok
  let getP :: KC.Permission -> Perm
      getP (KC.Permission rsname _ scopes) = Perm rsname (mapMaybe readScope scopes)
  return $ map getP ps 

postAuth :: AuthBody -> Waziup Token
postAuth (AuthBody username password) = do
  info "Post authentication"
  tok <- runKeycloak (getUserAuthToken username password)
  return tok

getSensors :: Maybe Token -> Maybe SensorsQuery -> Maybe SensorsLimit -> Maybe SensorsOffset -> Waziup [Sensor]
getSensors tok mq mlimit moffset = do
  info "Get sensors"
  entities <- runOrion $ O.getEntities mq
  let sensors = map getSensorFromEntity entities
  ps <- getPerms tok
  let sensors2 = filter (checkPermSensor SensorsView ps) sensors -- TODO limits
  return sensors2

checkPermSensor :: Scope -> [Perm] -> Sensor -> Bool
checkPermSensor scope perms sen = any (\p -> (permResource p) == (unSensorId $ senId sen) && scope `elem` (permScopes p)) perms

getSensor :: Maybe Token -> SensorId -> Waziup Sensor
getSensor tok (SensorId sid) = do
  info "Get sensor"
  sensor <- getSensorFromEntity <$> runOrion (O.getEntity $ EntityId sid)
  case (senKeycloakId sensor) of
    Just keyId -> do
      debug "Check permissions"
      runKeycloak $ checkPermission keyId (pack $ show SensorsView) tok
      debug "Permission granted, returning sensor"
      return sensor
    Nothing -> do
      err "Error, sensor does not have a Keycloak ID"
      throwError err500 {errBody = "Not authorized"}

postSensor :: Maybe Token -> Sensor -> Waziup NoContent
postSensor tok s@(Sensor (SensorId sid) _ _ _ _ vis _ _ _ _ _) = do
  info $ "Post sensor: " ++ (show s)
  debug "Check permissions"
  runKeycloak $ checkPermission (ResourceId "Sensors") (pack $ show SensorsCreate) tok
  debug "Create entity"
  let username = case tok of
       Just t -> getUsername t
       Nothing -> Just "guest"
  debug $ "Onwer: " <> (show username)
  let entity = getEntityFromSensor (s {senOwner = username})
  res2 <- C.try $ runOrion $ O.postEntity entity 
  case res2 of
    Right _ -> do 
      let res = KC.Resource {
         resId      = Nothing,
         resName    = sid,
         resType    = Nothing,
         resUris    = [],
         resScopes  = map (pack.show) [SensorsView, SensorsUpdate, SensorsDelete, SensorsDataCreate, SensorsDataView],
         resOwner   = Owner Nothing "cdupont",
         resOwnerManagedAccess = True,
         resAttributes = if (isJust vis) then [KC.Attribute "visibility" [pack $ show $ fromJust vis]] else []}
      keyRes <- C.try $ runKeycloak $ createResource res tok
      case keyRes of
        Right (ResourceId resId) -> do
          runOrion $ O.postTextAttributeOrion (EntityId sid) (AttributeId "keycloak_id") resId
          return NoContent
        Left err -> do
          error $ "Keycloak error: " ++ (show err) ++ " deleting sensor"
          (_ :: Either ServantErr ()) <- C.try $ runOrion $ O.deleteEntity (EntityId sid)
          throwError err
    Left (err :: ServantErr)  -> do
      warn "Orion error"
      throwError err 
 
deleteSensor :: Maybe Token -> SensorId -> Waziup NoContent
deleteSensor tok (SensorId sid) = do
  info "Delete sensor"
  withKCId sid $ \keyId -> do
    debug "Check permissions"
    runKeycloak $ checkPermission keyId (pack $ show SensorsDelete) tok
    debug "Delete Keycloak resource"
    runKeycloak $ deleteResource keyId tok
    debug "Delete Orion resource"
    runOrion $ O.deleteEntity (EntityId sid)
  return NoContent

putSensorLocation :: Maybe Token -> SensorId -> Location -> Waziup NoContent
putSensorLocation mtok (SensorId sid) loc = do
  info $ "Put sensor location: " ++ (show loc)
  withKCId sid $ \keyId -> do
    debug "Check permissions"
    runKeycloak $ checkPermission keyId (pack $ show SensorsUpdate) mtok
    debug "Update Orion resource"
    let att = getLocationAttr loc
    runOrion $ O.postAttribute (EntityId sid) att 
  return NoContent

putSensorName :: Maybe Token -> SensorId -> SensorName -> Waziup NoContent
putSensorName mtok (SensorId sid) name = do
  info $ "Put sensor name: " ++ (show name)
  withKCId sid $ \keyId -> do
    debug "Check permissions"
    runKeycloak $ checkPermission keyId (pack $ show SensorsUpdate) mtok
    debug "Update Orion resource"
    runOrion $ O.postTextAttributeOrion (EntityId sid) (AttributeId "name") name
  return NoContent

putSensorGatewayId :: Maybe Token -> SensorId -> GatewayId -> Waziup NoContent
putSensorGatewayId mtok (SensorId sid) (GatewayId gid) = do
  info $ "Put sensor gateway ID: " ++ (show gid)
  withKCId sid $ \keyId -> do
    debug "Check permissions"
    runKeycloak $ checkPermission keyId (pack $ show SensorsUpdate) mtok
    debug "Update Orion resource"
    runOrion $ O.postTextAttributeOrion (EntityId sid) (AttributeId "gateway_id") gid
  return NoContent

putSensorVisibility :: Maybe Token -> SensorId -> Visibility -> Waziup NoContent
putSensorVisibility mtok (SensorId sid) vis = do
  info $ "Put sensor visibility: " ++ (show vis)
  withKCId sid $ \keyId -> do
    debug "Check permissions"
    runKeycloak $ checkPermission keyId (pack $ show SensorsUpdate) mtok
    debug "Update Orion resource"
    runOrion $ O.postTextAttributeOrion (EntityId sid) (AttributeId "visibility") (convertString $ show vis)
  return NoContent

-- * From Orion to Waziup types

getSensorFromEntity :: O.Entity -> Sensor
getSensorFromEntity (O.Entity (EntityId eId) etype attrs) = 
  Sensor { senId           = SensorId eId,
           senGatewayId    = GatewayId <$> O.fromSimpleAttribute (AttributeId "gateway_id") attrs,
           senName         = fromSimpleAttribute (AttributeId "name") attrs,
           senOwner        = fromSimpleAttribute (AttributeId "owner") attrs,
           senLocation     = getLocation attrs,
           senDomain       = fromSimpleAttribute (AttributeId "domain") attrs,
           senVisibility   = fromSimpleAttribute (AttributeId "visibility") attrs >>= readVisibility,
           senDateCreated  = fromSimpleAttribute (AttributeId "dateCreated") attrs >>= parseISO8601.unpack,
           senDateModified = fromSimpleAttribute (AttributeId "dateModified") attrs >>= parseISO8601.unpack,
           senMeasurements = mapMaybe getMeasurementFromAttribute attrs,
           senKeycloakId   = ResourceId <$> O.fromSimpleAttribute (AttributeId "keycloak_id") attrs}

getLocation :: [O.Attribute] -> Maybe Location
getLocation attrs = do 
    (O.Attribute _ _ mval _) <- L.find (\(O.Attribute attId _ _ _) -> attId == (AttributeId "location")) attrs
    (Object o) <- mval
    (Array a) <- lookup "coordinates" $ H.toList o
    let [Number lon, Number lat] = V.toList a
    return $ Location (Latitude $ toRealFloat lat) (Longitude $ toRealFloat lon)

getMeasurementFromAttribute :: O.Attribute -> Maybe Measurement
getMeasurementFromAttribute (O.Attribute (AttributeId name) aType val mets) =
  if (aType == "Measurement") 
    then Just $ Measurement { measId            = MeasId name,
                              measName          = fromSimpleMetadata (MetadataId "name") mets,
                              measQuantityKind  = QuantityKindId <$> fromSimpleMetadata (MetadataId "quantity_kind") mets,
                              measSensorKind    = SensorKindId <$> fromSimpleMetadata (MetadataId "sensing_device") mets,
                              measUnit          = UnitId <$> fromSimpleMetadata (MetadataId "unit") mets,
                              measLastValue     = getMeasLastValue val mets}
    else Nothing

getMeasLastValue :: Maybe Value -> [O.Metadata] -> Maybe MeasurementValue
getMeasLastValue mval mets = do
   value <- mval
   guard $ not $ isNull value
   return $ MeasurementValue value 
                             (O.fromSimpleMetadata (MetadataId "timestamp")    mets >>= parseISO8601.unpack)
                             (O.fromSimpleMetadata (MetadataId "dateModified") mets >>= parseISO8601.unpack)


isNull :: Value -> Bool
isNull Null = True
isNull _    = False


-- * From Waziup to Orion types

getEntityFromSensor :: Sensor -> O.Entity
getEntityFromSensor (Sensor (SensorId sid) sgid sname sloc sdom svis meas sown _ _ skey) = 
  O.Entity (EntityId sid) "SensingDevice" $ catMaybes [getSimpleAttr (AttributeId "name")        <$> sname,
                                                       getSimpleAttr (AttributeId "gateway_id")  <$> (unGatewayId <$> sgid),
                                                       getSimpleAttr (AttributeId "owner")       <$> sown,
                                                       getSimpleAttr (AttributeId "domain")      <$> sdom,
                                                       getSimpleAttr (AttributeId "keycloak_id") <$> (unResId <$> skey),
                                                       getSimpleAttr (AttributeId "visibility")  <$> ((pack.show) <$> svis),
                                                       getLocationAttr               <$> sloc] <>
                                                       map getAttributeFromMeasurement meas

getLocationAttr :: Location -> O.Attribute
getLocationAttr (Location (Latitude lat) (Longitude lon)) = O.Attribute (AttributeId "location") "geo:json" (Just $ object ["type" .= ("Point" :: Text), "coordinates" .= [lon, lat]]) []

getAttributeFromMeasurement :: Measurement -> O.Attribute
getAttributeFromMeasurement (Measurement (MeasId measId) name sd qk u lv) = 
  (O.Attribute (AttributeId measId) "Measurement"
                     (measValue <$> lv)
                     (catMaybes [getTextMetadata (MetadataId "name")           <$> name,
                                 getTextMetadata (MetadataId "quantity_kind")  <$> unQuantityKindId <$> qk,
                                 getTextMetadata (MetadataId "sensing_device") <$> unSensorKindId <$> sd,
                                 getTextMetadata (MetadataId "unit")           <$> unUnitId <$> u]))


withKCId :: Text -> (ResourceId -> Waziup a) -> Waziup a
withKCId sid f = do
  sensor <- getSensorFromEntity <$> runOrion (O.getEntity $ EntityId sid)
  case (senKeycloakId sensor) of
    Just keyId -> f keyId 
    Nothing -> do
      error "Cannot delete sensor: KC Id not present"
      throwError err500 {errBody = "Cannot delete sensor: KC Id not present"}


-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Sensors" s
info  s = liftIO $ infoM    "Sensors" s
warn  s = liftIO $ warningM "Sensors" s
err   s = liftIO $ errorM   "Sensors" s

