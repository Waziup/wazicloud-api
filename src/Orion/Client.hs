
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Orion.Client where

import Network.Wreq
import Control.Lens
import Data.Aeson as JSON
import Data.Aeson.BetterErrors as AB
import Data.Text hiding (head, tail, find, map, filter)
import GHC.Generics (Generic)
import Data.Maybe
import Waziup.Types
import qualified Data.HashMap.Strict as H
import Debug.Trace
import Control.Monad.Reader
import Data.Aeson.BetterErrors.Internal
import Data.Time.ISO8601
import Data.Foldable as F
import qualified Data.Vector as V
import Data.Scientific


data Entity = Entity {
  entId :: Text,
  entType :: Text,
  entAttributes :: [(Text, Attribute)]
  } deriving (Generic, Show)

getEntity :: Parse e Entity
getEntity = do
    eId    <- AB.key "id" asText
    eType <- AB.key "type" asText
    attrs <- forEachInObject getAtt
    return $ Entity eId eType (catMaybes attrs) where
      getAtt "id" = return Nothing 
      getAtt "type" = return Nothing 
      getAtt k = do
        a <- getAttribute
        return $ Just (k, a)

data Attribute = Attribute {
  attType :: Text,
  attValue :: Maybe Value,
  attMetadata :: [(Text, Metadata)]
  } deriving (Generic, Show)

getAttribute :: Parse e Attribute
getAttribute = do
    (ParseReader _ t) <- ask
    aType  <- AB.key "type" asText
    aValue <- AB.keyMay "value" AB.asValue
    mets   <- AB.keyMay "metadata" getMetadatas
    return $ Attribute aType aValue (F.concat mets)

data Metadata = Metadata {
  metType :: Maybe Text,
  metValue :: Maybe Value
  } deriving (Generic, Show)

getMetadatas :: Parse e [(Text, Metadata)]
getMetadatas = forEachInObject $ \a -> do
  m <- getMetadata
  return (a, m)

getMetadata :: Parse e Metadata
getMetadata = Metadata <$> AB.keyMay "type" asText
                       <*> AB.keyMay "value" AB.asValue
opts = defaults &
       header "Fiware-Service" .~ ["waziup"] &
       param  "attrs"          .~ ["dateModified,dateCreated,*"] &
       param  "metadata"       .~ ["dateModified,dateCreated,*"] 

getSensorsOrion :: IO [Sensor]
getSensorsOrion = do
  res <- getWith opts "http://localhost:1026/v2/entities"
  let res2 = fromJust $ res ^? responseBody
  case AB.parse (eachInArray getEntity) res2 of
     Right es -> do
       return $ mapMaybe getSensor es
     Left err -> do
       mapM_ (putStrLn.unpack) (displayError' err)
       putStrLn $ "Error while decoding JSON: " ++ (show err)
       return []

getSensorOrion :: Text -> IO (Maybe Sensor)
getSensorOrion id = do
  res <- getWith opts $ "http://localhost:1026/v2/entities/" ++ unpack id
  let res2 = fromJust $ res ^? responseBody
  case AB.parse getEntity res2 of
     Right es -> do
       return $ getSensor es
     Left err -> do
       mapM_ (putStrLn.unpack) (displayError' err)
       putStrLn $ "Error while decoding JSON: " ++ (show err)
       return Nothing

getSensor :: Entity -> Maybe Sensor
getSensor (Entity eId etype attrs) = if etype == "SensingDevice" then Just sensor  else Nothing where
  sensor = Sensor { sensorId           = eId,
                    sensorGatewayId    = getSimpleAttribute "gateway_id" attrs,
                    sensorName         = getSimpleAttribute "name" attrs,
                    sensorOwner        = getSimpleAttribute "owner" attrs,
                    sensorLocation     = getLocation attrs,
                    sensorDomain       = getSimpleAttribute "domain" attrs,
                    sensorVisibility   = getSimpleAttribute "visibility" attrs >>= readVisibility,
                    sensorDateCreated  = getSimpleAttribute "dateCreated" attrs >>= parseISO8601.unpack,
                    sensorDateUpdated  = getSimpleAttribute "dateModified" attrs >>= parseISO8601.unpack,
                    sensorMeasurements = getMeasurements attrs}
                         
getSimpleAttribute :: Text -> [(Text, Attribute)] -> Maybe Text
getSimpleAttribute attName attrs = Just $ s where 
   (Just (Just (String s))) = attValue <$> lookup attName attrs
   _ = Nothing

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

