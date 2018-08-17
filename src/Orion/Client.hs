
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Orion.Client where

import Network.Wreq
import Control.Lens
import Control.Monad
import Data.Aeson as JSON
-- import Data.Aeson.BetterErrors as AB
import Data.Aeson.Types
import Data.Aeson.Lens
import Data.Map (Map)
import Data.Text hiding (head, tail, find, map, filter)
import GHC.Generics (Generic)
import Network.HTTP.QueryString
import Data.ByteString.Base64 as B64
import Data.Text.Encoding
import Data.Maybe
import Data.Foldable
import Waziup.Types
import Data.List
import qualified Data.HashMap.Strict as H
import Control.Monad.IO.Class
import Debug.Trace

data Entity = Entity {
  entId :: Text,
  entType :: Text,
  entAttributes :: [(Text, Attribute)]
  } deriving (Generic, Show)

instance FromJSON Entity where
  parseJSON = withObject "" $ \o -> do
    id    <- o .: "id"
    eType <- o .: "type"
    attrs <- getAttrs o
    return $ Entity id eType attrs 

getAttrs :: Object -> Parser [(Text, Attribute)]
getAttrs v = do
  let fil = filter (\(key, val) -> key /= "id" && key /= "type") $ H.toList v
  mapM (\(key, val) -> do v <- parseJSON val; return (key, v)) fil

data Attribute = Attribute {
  attType :: Text,
  attValue :: Maybe Text,
  attMetadata :: [(Text, Metadata)]
  } deriving (Generic, Show)

instance FromJSON Attribute where
  parseJSON = withObject "" $ \o -> do
    attType  <- o .: "type"
    attValue <- o .: "value"
    attMet   <- o .: "metadata"
    mets <- getMetadata attMet
    return $ Attribute attType attValue mets

getMetadata :: Object -> Parser [(Text, Metadata)]
getMetadata v = do
  let fil = filter (\(key, val) -> key /= "id" && key /= "type") $ H.toList v
  mapM (\(key, val) -> do v <- parseJSON val; return (key, v)) fil

data Metadata = Metadata {
  metType :: Text,
  metValue :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON Metadata where
  parseJSON (Object v) = trace ("Metadata" ++ (show v)) $ Metadata <$> v .: "type"
                                  <*> v .:? "value"
  parseJSON (Array _) = fail "array"
  parseJSON (String s) = return $ Metadata "string" (Just s)
  parseJSON (Number _) = fail "num"
  parseJSON (Bool _) = fail "bool"
  parseJSON (Null) = fail "null"

getSensorsOrion :: IO [Sensor]
getSensorsOrion = do
  let opts = defaults &
             header "Fiware-Service" .~ ["waziup"] &
             param "attrs"    .~ ["dateModified,dateCreated,*"] &
             param "metadata" .~ ["dateModified,dateCreated,*"] 
  res <- getWith opts "http://localhost:1026/v2/entities"
  let res2 = fromJust $ res ^? responseBody
  putStrLn $ show $ (JSON.decode res2 :: Maybe JSON.Value)
  case JSON.eitherDecode res2 of
     Right es -> return $ mapMaybe getSensor es
     Left err -> do
       putStrLn $ "Error while decoding JSON: " ++ err
       return []

getSensor :: Entity -> Maybe Sensor
getSensor (Entity id etype attrs) = if etype == "SensingDevice" then Just sensor  else Nothing where
  sensor = Sensor { sensorId = id,
                    sensorGatewayId = getSimpleAttribute "gateway_id" attrs,
                    sensorName = getSimpleAttribute "name" attrs,
                    sensorOwner = getSimpleAttribute "owner" attrs,
                    sensorLocation = getLocation attrs,
                    sensorDomain = getSimpleAttribute "domain" attrs,
                    sensorDateCreated = read . unpack <$> getSimpleAttribute "dateCreated" attrs,
                    sensorDateUpdated = read . unpack <$> getSimpleAttribute "dateModified" attrs,
                    sensorMeasurements = getMeasurements attrs}
                         
getSimpleAttribute :: Text -> [(Text, Attribute)] -> Maybe Text
getSimpleAttribute attName attrs = join $ attValue <$> lookup attName attrs

getMeasurements :: [(Text, Attribute)] -> [Measurement]
getMeasurements attrs = mapMaybe getMeas attrs where 
  getMeas (name, Attribute aType val mets) = if (aType == "Measurement") 
     then Just $ Measurement { measId = name,
                               measName = getSimpleMetadata "name" mets,
                               measQuantityKind = getSimpleMetadata "quantity_kind" mets,
                               measSensingDevice = getSimpleMetadata "sensind_device" mets,
                               measUnit = getSimpleMetadata "unit" mets,
                               measLastValue = if (isJust val) then getMeasLastValue (fromJust val) mets else Nothing}
     else Nothing

getLocation :: [(Text, Attribute)] -> Maybe Location
getLocation attrs = Nothing --do 
    --(Attribute _ _ val _) <- find (\(Attribute name aType _ _) -> name == "location" ) attrs
    --lat <- getSimpleMetadata "latitude" mets
    --lon <- getSimpleMetadata "longitude" mets
    --return $ Just $ Location {latitude = lat, longitude = lon}
 
getSimpleMetadata :: Text -> [(Text, Metadata)] -> Maybe Text
getSimpleMetadata name mets = join $ metValue <$> lookup name mets

getMeasLastValue :: Text -> [(Text, Metadata)] -> Maybe MeasurementValue
getMeasLastValue value mets = Nothing --if (isJust value) 
--  then Just $ MeasurementValue value (read $ getSimpleMetadata "timestamp" mets) (read $ getSimpleMetadata "date_received" mets)
--  else Nothing

