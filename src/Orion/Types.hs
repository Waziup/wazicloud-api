{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module Orion.Types where

import           Network.Wreq as W
import           Control.Lens hiding ((.=))
import           Data.Aeson as JSON
import           Data.Aeson.BetterErrors as AB
import           Data.Aeson.Casing
import           Data.Aeson.Types
import           Data.Text hiding (head, tail, find, map, filter)
import           Data.Text.Encoding
import           GHC.Generics (Generic)
import           Data.Maybe
import           Data.Monoid
import           Control.Monad.Reader
import           Control.Lens hiding ((.=))
import           Data.Foldable as F
import           Network.HTTP.Client (HttpException)
import           Control.Monad.Except (ExceptT)
import qualified Data.HashMap.Lazy as HML 


-- * Orion monad

type Orion a = ReaderT OrionConfig (ExceptT OrionError IO) a

data OrionError = HTTPError HttpException  -- ^ Keycloak returned an HTTP error.
                | ParseError Text          -- ^ Failed when parsing the response
                | EmptyError               -- ^ Empty error to serve as a zero element for Monoid.

-- * Orion config

data OrionConfig = OrionConfig {
  _orionUrl      :: Text,
  _fiwareService :: Text} deriving (Show, Eq)

defaultOrionConfig = OrionConfig {
  _orionUrl      = "http://localhost:1026",
  _fiwareService = "waziup"}

-- * Entities

newtype EntityId = EntityId {unEntityId :: Text} deriving (Show, Eq, Generic, ToJSON, FromJSON)
type EntityType = Text

data Entity = Entity {
  entId         :: EntityId,
  entType       :: EntityType,
  entAttributes :: [Attribute]
  } deriving (Generic, Show)

instance ToJSON Entity where
   toJSON (Entity entId entType attrs) = 
     merge_aeson $ (object ["id" .= entId, "type" .= entType]) : (map toJSON attrs)

parseEntity :: Parse e Entity
parseEntity = do
    eId   <- AB.key "id" asText
    eType <- AB.key "type" asText
    attrs <- catMaybes <$> forEachInObject parseAtt
    return $ Entity (EntityId eId) eType attrs where
      parseAtt "id" = return Nothing 
      parseAtt "type" = return Nothing 
      parseAtt k = Just <$> parseAttribute (AttributeId k)

-- * Attributes

newtype AttributeId = AttributeId {unAttributeId :: Text} deriving (Show, Eq, Generic, ToJSON, FromJSON)
type AttributeType = Text

data Attribute = Attribute {
  attId       :: AttributeId,
  attType     :: AttributeType,
  attValue    :: Maybe Value,
  attMetadata :: [Metadata]
  } deriving (Generic, Show)

instance ToJSON Attribute where
   toJSON (Attribute (AttributeId attId) attType attVal mets) = 
     object [ attId .= 
        (object $ ["type" .= attType, 
                   "value" .= attVal,
                   "metadata" .= merge_aeson (map toJSON mets)])]

parseAttribute :: AttributeId -> Parse e Attribute
parseAttribute attId = do
    aType  <- AB.key    "type" asText
    aValue <- AB.keyMay "value" AB.asValue
    mets   <- AB.keyMay "metadata" (forEachInObject (parseMetadata.MetadataId))
    return $ Attribute attId aType aValue (F.concat mets)

-- * Metadata

newtype MetadataId = MetadataId {unMeetadataId :: Text} deriving (Show, Eq, Generic, ToJSON, FromJSON)
type MetadataType = Text

data Metadata = Metadata {
  metId    :: MetadataId,
  metType  :: Maybe MetadataType,
  metValue :: Maybe Value
  } deriving (Generic, Show)

instance ToJSON Metadata where
   toJSON (Metadata (MetadataId id) mtyp mval) = 
     object [ id .= 
       (object $ catMaybes [("type",) <$> toJSON <$> mtyp,
                            ("value",) <$> toJSON <$> mval]) ]

parseMetadata :: MetadataId -> Parse e Metadata
parseMetadata mid = Metadata <$> pure mid
                             <*> AB.keyMay "type" AB.asText
                             <*> AB.keyMay "value" AB.asValue

-- Miscellaneous

type Path = Text

merge_aeson :: [Value] -> Value
merge_aeson = Object . HML.unions . map (\(Object x) -> x)

makeLenses ''OrionConfig
