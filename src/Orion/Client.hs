{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Orion.Client where

import           Network.Wreq as W
import           Network.Wreq.Types
import           Network.HTTP.Client (HttpException)
import           Network.HTTP.Types.Method
import           Network.HTTP.Types
import           Data.Aeson as JSON
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
import           Control.Lens hiding ((.=))
import           Control.Monad.Reader
import           Control.Monad.Except (ExceptT, throwError, MonadError, catchError)
import           Control.Exception hiding (try)
import qualified Control.Monad.Catch as C
import           Orion.Types
import           System.Log.Logger
import           GHC.Generics (Generic)
import           Waziup.Types
import           Debug.Trace

getSensorsOrion :: Maybe Text -> Maybe Int -> Maybe Int -> Orion [Sensor]
getSensorsOrion mq mlimit moffset = do
  let (query :: Query) = [("type", Just ("SensingDevice" :: C8.ByteString)),
               ("q", encodeUtf8 <$> mq),
               ("limit", C8.pack . show <$> mlimit),
               ("offset", C8.pack . show <$> moffset)]
  ents <- orionGet (decodeUtf8 $ "v2/entities" <> (renderQuery True query))  (eachInArray parseEntity)
  return $ map getSensor ents

getSensorOrion :: EntityId -> Orion Sensor
getSensorOrion eid = do
  ent <- orionGet ("/v2/entities/" <> eid) parseEntity
  return $ getSensor ent

postSensorOrion :: Sensor -> Orion ()
postSensorOrion s = do
  debug $ C8.unpack $ "Create sensor in Orion: " <> (BSL.toStrict $ JSON.encode s)
  let entity = getEntity s
  debug $ C8.unpack $ "Entity: " <> (BSL.toStrict $ JSON.encode entity)
  orionPost "/v2/entities" (toJSON entity)

deleteSensorOrion :: EntityId -> Orion ()
deleteSensorOrion eid = orionDelete ("/v2/entities/" <> eid)

-- Perform request to Orion.
orionGet :: (Show b) => Path -> Parse Text b -> Orion b
orionGet path parser = do 
  orionOpts@(OrionConfig baseUrl _ _ _) <- ask 
  let opts = defaults &
       header "Fiware-Service" .~ [encodeUtf8 $ fiwareService orionOpts] &
       param  "attrs"          .~ [attrs orionOpts] &
       param  "metadata"       .~ [metadata orionOpts] 
  let url = (unpack $ baseUrl <> path) 
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
  orionOpts@(OrionConfig baseUrl _ _ _) <- ask 
  let opts = defaults &
       header "Fiware-Service" .~ [encodeUtf8 $ fiwareService orionOpts] &
       param  "attrs"          .~ [attrs orionOpts] &
       param  "metadata"       .~ [metadata orionOpts] 
  let url = (unpack $ baseUrl <> path) 
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
  orionOpts@(OrionConfig baseUrl _ _ _) <- ask 
  let opts = defaults &
       header "Fiware-Service" .~ [encodeUtf8 $ fiwareService orionOpts] &
       param  "attrs"          .~ [attrs orionOpts] &
       param  "metadata"       .~ [metadata orionOpts] 
  let url = (unpack $ baseUrl <> path) 
  info $ "Issuing ORION DELETE with url: " ++ (show url) 
  debug $ "  headers: " ++ (show $ opts ^. W.headers) 
  eRes <- C.try $ liftIO $ W.deleteWith opts url
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
                                              senDateUpdated  = getSimpleAttribute "dateModified" attrs >>= parseISO8601.unpack,
                                              senMeasurements = getMeasurements attrs,
                                              senKeycloakId   = getSimpleAttribute "keycloak_id" attrs}
                         
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
    let [Number lat, Number lon] = V.toList a
    return $ Location (toRealFloat lat) (toRealFloat lon)
 
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
   return $ MeasurementValue value 
                             (getSimpleMetadata "timestamp" mets    >>= parseISO8601.unpack)
                             (getSimpleMetadata "dateModified" mets >>= parseISO8601.unpack)

getEntity :: Sensor -> Entity
getEntity (Sensor sid sgid sname sown meas sloc sdom _ _ svis skey) = 
  Entity sid "SensingDevice" $ catMaybes [getSimpleAttr "name" sname,
                              getSimpleAttr "gateway_id" sgid,
                              getSimpleAttr "owner" sown,
                              getSimpleAttr "domain" sown,
                              getSimpleAttr "keycloak_id" skey,
                              getSimpleAttr "visibility" ((pack.show) <$> svis),
                              getLocationAttr sloc] <>
                              map getMeasurementAttr meas

getSimpleAttr :: Text -> Maybe Text -> Maybe (Text, Attribute)
getSimpleAttr name (Just val) = Just (name, Attribute "String" (Just $ toJSON val) [])
getSimpleAttr _ Nothing = Nothing

getLocationAttr :: Maybe Location -> Maybe (Text, Attribute)
getLocationAttr (Just (Location lat lon)) = Just ("location", Attribute "geo:json" (Just $ object ["type" .= ("Point" :: Text), "coordinates" .= [lon, lat]]) [])
getLocationAttr Nothing = Nothing

getMeasurementAttr :: Measurement -> (Text, Attribute)
getMeasurementAttr (Measurement measId name sd qk u lv) = 
  (measId, Attribute "Measurement"
                     (measValue <$> lv)
                     [("name", Metadata (Just "String") (Just $ toJSON name)),
                      ("quantity_kind", Metadata (Just "String") (Just $ toJSON qk)),
                      ("sensing_device", Metadata (Just "String") (Just $ toJSON sd)),
                      ("unit", Metadata (Just "String") (Just $ toJSON u))])

debug, warn, info, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM "Orion" s
info s = liftIO $ infoM "Orion" s
warn s = liftIO $ warningM "Orion" s
err s = liftIO $ errorM "Orion" s

try :: MonadError a m => m b -> m (Either a b)
try act = catchError (Right <$> act) (return . Left)

