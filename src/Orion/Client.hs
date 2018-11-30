{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Orion.Client where

import           Network.Wreq as W
import           Network.Wreq.Types
import           Network.HTTP.Client (HttpException)
import           Network.HTTP.Types.Method
import           Network.HTTP.Types
import           Data.Aeson as JSON hiding (Options)
import           Data.Aeson.BetterErrors as AB
import           Data.Aeson.Casing
import           Data.Text hiding (head, tail, find, map, filter)
import           Data.Text.Encoding as TE
import           Data.Maybe
import           Data.Aeson.BetterErrors.Internal
import           Data.Time.ISO8601
import           Data.Foldable as F
import           Data.Scientific
import           Data.Monoid
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BSL
import           Data.String.Conversions
import           Control.Lens hiding ((.=))
import           Control.Monad.Reader
import           Control.Monad.Except (ExceptT, throwError, MonadError, catchError)
import           Control.Exception hiding (try)
import qualified Control.Monad.Catch as C
import           Orion.Types
import           Keycloak.Types (ResourceId(..))
import           System.Log.Logger
import           GHC.Generics (Generic)
import           Waziup.Types
import           Debug.Trace

getSensorsOrion :: Maybe Text -> Maybe Int -> Maybe Int -> Orion [Sensor]
getSensorsOrion mq mlimit moffset = do
  let qq = case mq of
       Just q -> [("q", Just $ encodeUtf8 q)]
       Nothing -> []
  let limitq = case mlimit of
       Just limit -> [("limit", Just $ C8.pack $ show limit)]
       Nothing -> []
  let offsetq = case moffset of
       Just offset -> [("offset", Just $ C8.pack $ show offset)]
       Nothing -> []
  let (query :: Query) = qq ++ limitq ++ offsetq ++ [("type", Just ("SensingDevice" :: C8.ByteString))]
  ents <- orionGet (decodeUtf8 $ "/v2/entities" <> (renderQuery True query))  (eachInArray parseEntity)
  return $ map getSensor ents

getSensorOrion :: EntityId -> Orion Sensor
getSensorOrion eid = do
  ent <- orionGet ("/v2/entities/" <> eid) parseEntity
  return $ getSensor ent

postSensorOrion :: Sensor -> Orion ()
postSensorOrion s = do
  debug $ C8.unpack $ "Create sensor in Orion: " <> (BSL.toStrict $ JSON.encode s)
  let entity = getEntity s
  debug $ convertString $ "Entity: " <> (JSON.encode entity)
  orionPost "/v2/entities" (toJSON entity)

deleteSensorOrion :: EntityId -> Orion ()
deleteSensorOrion eid = orionDelete ("/v2/entities/" <> eid)

postSensorKeycloakOrion :: EntityId -> ResourceId -> Orion ()
postSensorKeycloakOrion eid (ResourceId res) = do
  debug $ convertString $ "put Keycloak ID in Orion: " <> res
  orionPost ("/v2/entities/" <> eid <> "/attrs") (object ["keycloak_id" .= (toJSON $ (Attribute "String" (Just $ toJSON res) []) :: Value)])

putSensorNameOrion :: EntityId -> SensorName -> Orion ()
putSensorNameOrion eid name = do
  debug $ convertString $ "put Sensor name in Orion: " <> name
  orionPost ("/v2/entities/" <> eid <> "/attrs") (object ["name" .= (toJSON $ (Attribute "String" (Just $ toJSON name) []) :: Value)])

putSensorLocationOrion :: EntityId -> Location -> Orion ()
putSensorLocationOrion eid loc = do
  debug $ convertString $ "put Sensor location in Orion: " <> (show $ encode loc)
  let (field, val) = getLocationAttr loc
  orionPost ("/v2/entities/" <> eid <> "/attrs") (object [field .= (toJSON val)])

-- Get Orion URI and options
getOrionDetails :: Path -> Orion (String, Options)
getOrionDetails path = do
  orionOpts@(OrionConfig baseUrl service) <- ask 
  let opts = defaults &
       header "Fiware-Service" .~ [convertString service] &
       param  "attrs"          .~ ["dateModified,dateCreated,*"] &
       param  "metadata"       .~ ["dateModified,dateCreated,*"] 
  let url = (unpack $ baseUrl <> path) 
  return (url, opts)

-- Perform request to Orion.
orionGet :: (Show b) => Path -> Parse Text b -> Orion b
orionGet path parser = do 
  (url, opts) <- getOrionDetails path 
  info $ "Issuing ORION GET with url: " ++ (show url) 
  debug $ "  headers: " ++ (show $ opts ^. W.headers) 
  eRes <- C.try $ liftIO $ W.getWith opts url
  case eRes of 
    Right res -> do
      let body = fromJust $ res ^? responseBody
      case AB.parse parser body of
        Right ret -> do
          debug $ "Orion result: " ++ (show ret)
          return ret
        Left err2 -> do
          err $ "Orion parse error: " ++ (show err2)
          throwError $ ParseError $ pack (show err2)
    Left err -> do
      warn $ "Orion HTTP Error: " ++ (show err)
      throwError $ HTTPError err

orionPost :: (Postable dat, Show dat) => Path -> dat -> Orion ()
orionPost path dat = do 
  (url, opts) <- getOrionDetails path 
  info $ "Issuing ORION POST with url: " ++ (show url) 
  debug $ "  data: " ++ (show dat) 
  debug $ "  headers: " ++ (show $ opts ^. W.headers) 
  eRes <- C.try $ liftIO $ W.postWith opts url dat
  case eRes of 
    Right res -> return ()
    Left err -> do
      warn $ "Orion HTTP Error: " ++ (show err)
      throwError $ HTTPError err

orionDelete :: Path -> Orion ()
orionDelete path = do 
  (url, opts) <- getOrionDetails path 
  info $ "Issuing ORION DELETE with url: " ++ (show url) 
  debug $ "  headers: " ++ (show $ opts ^. W.headers) 
  eRes <- C.try $ liftIO $ W.deleteWith opts url
  case eRes of 
    Right res -> return ()
    Left err -> do
      warn $ "Orion HTTP Error: " ++ (show err)
      throwError $ HTTPError err

orionPut :: (Putable dat, Show dat) => Path -> dat -> Orion ()
orionPut path dat = do 
  (url, opts) <- getOrionDetails path 
  info $ "Issuing ORION PUT with url: " ++ (show url) 
  debug $ "  data: " ++ (show dat) 
  debug $ "  headers: " ++ (show $ opts ^. W.headers) 
  eRes <- C.try $ liftIO $ W.putWith opts url dat
  case eRes of 
    Right res -> return ()
    Left err -> do
      warn $ "Orion HTTP Error: " ++ (show err)
      throwError $ HTTPError err


-- * Helper functions

getSensor :: Entity -> Sensor
getSensor (Entity eId etype attrs) = Sensor { senId           = eId,
                                              senGatewayId    = getSimpleAttribute "gateway_id" attrs,
                                              senName         = getSimpleAttribute "name" attrs,
                                              senOwner        = getSimpleAttribute "owner" attrs,
                                              senLocation     = getLocation attrs,
                                              senDomain       = getSimpleAttribute "domain" attrs,
                                              senVisibility   = getSimpleAttribute "visibility" attrs >>= readVisibility,
                                              senDateCreated  = getSimpleAttribute "dateCreated" attrs >>= parseISO8601.unpack,
                                              senDateModified = getSimpleAttribute "dateModified" attrs >>= parseISO8601.unpack,
                                              senMeasurements = getMeasurements attrs,
                                              senKeycloakId   = ResourceId <$> getSimpleAttribute "keycloak_id" attrs}
                         
getSimpleAttribute :: Text -> [(Text, Attribute)] -> Maybe Text
getSimpleAttribute attName attrs = do
   (Attribute _ mval _) <- lookup attName attrs
   val <- mval
   getString val

getMeasurements :: [(Text, Attribute)] -> [Measurement]
getMeasurements attrs = mapMaybe getMeas attrs where 
  getMeas (name, Attribute aType val mets) = if (aType == "Measurement") 
     then Just $ Measurement { measId            = name,
                               measName          = getSimpleMetadata "name" mets,
                               measQuantityKind  = getSimpleMetadata "quantity_kind" mets,
                               measSensingDevice = getSimpleMetadata "sensing_device" mets,
                               measUnit          = getSimpleMetadata "unit" mets,
                               measLastValue     = getMeasLastValue val mets}
     else Nothing


getLocation :: [(Text, Attribute)] -> Maybe Location
getLocation attrs = do 
    (Attribute _ mval _) <- lookup "location" attrs
    (Object o) <- mval
    (Array a) <- lookup "coordinates" $ H.toList o
    let [Number lon, Number lat] = V.toList a
    return $ Location (Latitude $ toRealFloat lat) (Longitude $ toRealFloat lon)
 
getSimpleMetadata :: Text -> [(Text, Metadata)] -> Maybe Text
getSimpleMetadata name mets = do
   (Metadata _ mval) <- lookup name mets
   val <- mval
   getString val

getString :: Value -> Maybe Text
getString (String s) = Just s
getString _ = Nothing

getMeasLastValue :: Maybe Value -> [(Text, Metadata)] -> Maybe MeasurementValue
getMeasLastValue mval mets = do
   value <- mval
   guard $ not $ isNull value
   return $ MeasurementValue value 
                             (getSimpleMetadata "timestamp" mets    >>= parseISO8601.unpack)
                             (getSimpleMetadata "dateModified" mets >>= parseISO8601.unpack)

isNull :: Value -> Bool
isNull Null = True
isNull _    = False

getEntity :: Sensor -> Entity
getEntity (Sensor sid sgid sname sloc sdom svis meas sown _ _ skey) = 
  Entity sid "SensingDevice" $ catMaybes [getSimpleAttr "name"        <$> sname,
                                          getSimpleAttr "gateway_id"  <$> sgid,
                                          getSimpleAttr "owner"       <$> sown,
                                          getSimpleAttr "domain"      <$> sdom,
                                          getSimpleAttr "keycloak_id" <$> (unResId <$> skey),
                                          getSimpleAttr "visibility"  <$> ((pack.show) <$> svis),
                                          getLocationAttr             <$> sloc] <>
                                          map getMeasurementAttr meas

getSimpleAttr :: Text -> Text -> (Text, Attribute)
getSimpleAttr name val = (name, Attribute "String" (Just $ toJSON val) [])

getLocationAttr :: Location -> (Text, Attribute)
getLocationAttr (Location (Latitude lat) (Longitude lon)) = ("location", Attribute "geo:json" (Just $ object ["type" .= ("Point" :: Text), "coordinates" .= [lon, lat]]) [])

getMeasurementAttr :: Measurement -> (Text, Attribute)
getMeasurementAttr (Measurement measId name sd qk u lv) = 
  (measId, Attribute "Measurement"
                     (measValue <$> lv)
                     [("name", Metadata (Just "String") (Just $ toJSON name)),
                      ("quantity_kind", Metadata (Just "String") (Just $ toJSON qk)),
                      ("sensing_device", Metadata (Just "String") (Just $ toJSON sd)),
                      ("unit", Metadata (Just "String") (Just $ toJSON u))])

debug, warn, info, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Orion" s
info s  = liftIO $ infoM    "Orion" s
warn s  = liftIO $ warningM "Orion" s
err s   = liftIO $ errorM   "Orion" s

try :: MonadError a m => m b -> m (Either a b)
try act = catchError (Right <$> act) (return . Left)

