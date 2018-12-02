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
import qualified Orion as O
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
checkPermSensor scope perms sen = any (\p -> (permResource p) == (senId sen) && scope `elem` (permScopes p)) perms

getSensor :: Maybe Token -> SensorId -> Waziup Sensor
getSensor tok sid = do
  info "Get sensor"
  sensor <- getSensorFromEntity <$> runOrion (O.getEntity sid)
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
postSensor tok s@(Sensor sid _ _ _ _ vis _ _ _ _ _) = do
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
         resAttributes = if (isJust vis) then [Attribute "visibility" [pack $ show $ fromJust vis]] else []}
      keyRes <- C.try $ runKeycloak $ createResource res tok
      case keyRes of
        Right (ResourceId resId) -> do
          runOrion $ O.postTextAttributeOrion sid "keycloak_id" resId
          return NoContent
        Left err -> do
          error $ "Keycloak error: " ++ (show err) ++ " deleting sensor"
          (_ :: Either ServantErr ()) <- C.try $ runOrion $ O.deleteEntity sid
          throwError err
    Left (err :: ServantErr)  -> do
      warn "Orion error"
      throwError err 
 
deleteSensor :: Maybe Token -> SensorId -> Waziup NoContent
deleteSensor tok sid = do
  info "Delete sensor"
  withKCId sid $ \keyId -> do
    debug "Check permissions"
    runKeycloak $ checkPermission keyId (pack $ show SensorsDelete) tok
    debug "Delete Keycloak resource"
    runKeycloak $ deleteResource keyId tok
    debug "Delete Orion resource"
    runOrion $ O.deleteEntity sid
  return NoContent

putSensorLocation :: Maybe Token -> SensorId -> Location -> Waziup NoContent
putSensorLocation mtok sid loc = do
  info $ "Put sensor location: " ++ (show loc)
  withKCId sid $ \keyId -> do
    debug "Check permissions"
    runKeycloak $ checkPermission keyId (pack $ show SensorsUpdate) mtok
    debug "Update Orion resource"
    let (attId, att) = getLocationAttr loc
    runOrion $ O.postAttribute sid attId att 
  return NoContent

putSensorName :: Maybe Token -> SensorId -> SensorName -> Waziup NoContent
putSensorName mtok sid name = do
  info $ "Put sensor name: " ++ (show name)
  withKCId sid $ \keyId -> do
    debug "Check permissions"
    runKeycloak $ checkPermission keyId (pack $ show SensorsUpdate) mtok
    debug "Update Orion resource"
    runOrion $ O.postTextAttributeOrion sid "name" name
  return NoContent

putSensorGatewayId :: Maybe Token -> SensorId -> GatewayId -> Waziup NoContent
putSensorGatewayId mtok sid gid = do
  info $ "Put sensor gateway ID: " ++ (show gid)
  withKCId sid $ \keyId -> do
    debug "Check permissions"
    runKeycloak $ checkPermission keyId (pack $ show SensorsUpdate) mtok
    debug "Update Orion resource"
    runOrion $ O.postTextAttributeOrion sid "gateway_id" gid
  return NoContent

putSensorVisibility :: Maybe Token -> SensorId -> Visibility -> Waziup NoContent
putSensorVisibility mtok sid vis = do
  info $ "Put sensor visibility: " ++ (show vis)
  withKCId sid $ \keyId -> do
    debug "Check permissions"
    runKeycloak $ checkPermission keyId (pack $ show SensorsUpdate) mtok
    debug "Update Orion resource"
    runOrion $ O.postTextAttributeOrion sid "visibility" (convertString $ show vis)
  return NoContent

-- * From Orion to Waziup types

getSensorFromEntity :: O.Entity -> Sensor
getSensorFromEntity (O.Entity eId etype attrs) = 
  Sensor { senId           = eId,
           senGatewayId    = O.fromSimpleAttribute "gateway_id" attrs,
           senName         = O.fromSimpleAttribute "name" attrs,
           senOwner        = O.fromSimpleAttribute "owner" attrs,
           senLocation     = getLocation attrs,
           senDomain       = O.fromSimpleAttribute "domain" attrs,
           senVisibility   = O.fromSimpleAttribute "visibility" attrs >>= readVisibility,
           senDateCreated  = O.fromSimpleAttribute "dateCreated" attrs >>= parseISO8601.unpack,
           senDateModified = O.fromSimpleAttribute "dateModified" attrs >>= parseISO8601.unpack,
           senMeasurements = mapMaybe getMeasurementFromAttribute attrs,
           senKeycloakId   = ResourceId <$> O.fromSimpleAttribute "keycloak_id" attrs}

getLocation :: [(Text, O.Attribute)] -> Maybe Location
getLocation attrs = do 
    (O.Attribute _ mval _) <- lookup "location" attrs
    (Object o) <- mval
    (Array a) <- lookup "coordinates" $ H.toList o
    let [Number lon, Number lat] = V.toList a
    return $ Location (Latitude $ toRealFloat lat) (Longitude $ toRealFloat lon)

getMeasurementFromAttribute :: (Text, O.Attribute) -> Maybe Measurement
getMeasurementFromAttribute (name, O.Attribute aType val mets) =
  if (aType == "Measurement") 
    then Just $ Measurement { measId            = name,
                              measName          = O.fromSimpleMetadata "name" mets,
                              measQuantityKind  = O.fromSimpleMetadata "quantity_kind" mets,
                              measSensingDevice = O.fromSimpleMetadata "sensing_device" mets,
                              measUnit          = O.fromSimpleMetadata "unit" mets,
                              measLastValue     = getMeasLastValue val mets}
    else Nothing

getMeasLastValue :: Maybe Value -> [(Text, O.Metadata)] -> Maybe MeasurementValue
getMeasLastValue mval mets = do
   value <- mval
   guard $ not $ isNull value
   return $ MeasurementValue value 
                             (O.fromSimpleMetadata "timestamp" mets    >>= parseISO8601.unpack)
                             (O.fromSimpleMetadata "dateModified" mets >>= parseISO8601.unpack)


isNull :: Value -> Bool
isNull Null = True
isNull _    = False


-- * From Waziup to Orion types

getEntityFromSensor :: Sensor -> O.Entity
getEntityFromSensor (Sensor sid sgid sname sloc sdom svis meas sown _ _ skey) = 
  O.Entity sid "SensingDevice" $ catMaybes [O.getSimpleAttr "name"        <$> sname,
                                            O.getSimpleAttr "gateway_id"  <$> sgid,
                                            O.getSimpleAttr "owner"       <$> sown,
                                            O.getSimpleAttr "domain"      <$> sdom,
                                            O.getSimpleAttr "keycloak_id" <$> (unResId <$> skey),
                                            O.getSimpleAttr "visibility"  <$> ((pack.show) <$> svis),
                                            getLocationAttr               <$> sloc] <>
                                            map getAttributeFromMeasurement meas

getLocationAttr :: Location -> (Text, O.Attribute)
getLocationAttr (Location (Latitude lat) (Longitude lon)) = ("location", O.Attribute "geo:json" (Just $ object ["type" .= ("Point" :: Text), "coordinates" .= [lon, lat]]) [])

getAttributeFromMeasurement :: Measurement -> (Text, O.Attribute)
getAttributeFromMeasurement (Measurement measId name sd qk u lv) = 
  (measId, O.Attribute "Measurement"
                     (measValue <$> lv)
                     (catMaybes [O.getTextMetadata "name" <$> name,
                                 O.getTextMetadata "quantity_kind" <$> qk,
                                 O.getTextMetadata "sensing_device" <$> sd,
                                 O.getTextMetadata "unit" <$> u]))


withKCId :: SensorId -> (ResourceId -> Waziup a) -> Waziup a
withKCId sid f = do
  sensor <- getSensorFromEntity <$> runOrion (O.getEntity sid)
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

