
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Orion.Client where

import Network.Wreq
import Control.Lens
import Control.Monad
import Data.Aeson as JSON
import Data.Aeson.Lens
import Data.Map (Map)
import Data.Text hiding (head, tail)
import GHC.Generics (Generic)
import Network.HTTP.QueryString
import Data.ByteString.Base64 as B64
import Data.Text.Encoding
import Data.Maybe
import Data.Foldable

data Entity = Entity {
  entId :: Text,
  entType :: Text,
  entAttributes :: [EntAttribute]
  } deriving (Generic, Show)

data Attribute = Attribute {
  attName :: Text,
  attType :: Text,
  attValue :: Maybe Text,
  attMetadata :: [AttMetadata]
  }

data Metadata = Metdata {
  metName :: Text,
  metType :: Text,
  metValue :: Maybe Text
  }

getSensorsOrion :: IO [Sensor]
getSensorsOrion = do
  let entities = get "http://localhost:1026/v2/entities"
  let sensors = mapMaybe getSensor entities

getSensor :: Entity -> Maybe Sensor
getSensor (Entity id etype attrs) = if etype == "SensingDevice" then Just sensor else Nothing where
  let sensor = Sensor { sensorId = id,
                        sensorGatewayId = getSimpleAttribute "gateway_id" attrs,
                        sensorName = getSimpleAttribute "name" attrs,
                        sensorOwner = getSimpleAttribute "owner" attrs,
                        sensorLocation = getSimpleAttribute "location" attrs,
                        sensorDomain = getSimpleAttribute "domain" attrs,
                        sensorDateCreated = getSimpleAttribute "dateCreated" attrs,
                        sensorDateUpdated = getSimpleAttribute "dateModified" attrs,
                        sensorMeasurements = getMeasurements attrs
                      }
                         
getSimpleAttribute :: Text -> [Attributes] -> Maybe Text
getSimpleAttribute attName attrs = attValue <$> find (/(Attribute name aType _ _) -> aType == "String" && name == attName) attrs

getMeasurements :: [Attribute] -> [Measurement]
getMeasurements attrs = mapMaybe getMeas attrs where 
  let getMeas (Attribute name aType val mets) = if (aType == "Measurement") 
     then Just $ Measurement {
                   measId = name,
                   measName = getSimpleMetadata "name" mets,
                   measQuantityKind = getSimpleMetadata "quantity_kind" mets,
                   measSensingDevice = getSimpleMetadata "sensind_device" mets,
                   measUnit = getSimpleMetadata "unit" mets,
                   measLastValue = getMeasLastValue val mets
                 }
     else Nothing

getSimpleMetadata :: Text -> [Metadata] -> Maybe Text
getSimpleMetadata name -> mets = metValue <$> find (/(Metadata name aType _) -> aType == "String" && name == attName) mets

getMeasLastValue :: Text -> [Metadata] -> Maybe MeasurementValue
getMeasLastValue value mets = if (isJust value) 
  then Just $ MeasurementValue value (read $ getSimpleMetadata "timestamp" mets) (read $ getSimpleMetadata "date_received" mets)
  else Nothing

