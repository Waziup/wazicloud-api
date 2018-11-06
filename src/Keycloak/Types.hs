{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Keycloak.Types where

import Data.Aeson as JSON
import Data.Aeson.Types
import Data.Aeson.Casing
import Data.Text hiding (head, tail, map)
import GHC.Generics (Generic)
import Data.Maybe
import Control.Monad
import Data.Foldable as F
import Data.Aeson.BetterErrors as AB

type ResourceId = Text
type ResourceName = Text
type ScopeId = Text
type ScopeName = Text
type Token = Text

type Scope = Text 

data Permission = Permission 
  { rsname :: ResourceName,
    rsid   :: ResourceId,
    scopes :: [Scope]
  } deriving (Generic, Show)

parsePermission :: Parse e Permission
parsePermission = do
    rsname  <- AB.key "rsname" asText
    rsid    <- AB.key "rsid" asText
    scopes  <- AB.keyMay "scopes" (eachInArray asText) 
    return $ Permission rsname rsid (if (isJust scopes) then (fromJust scopes) else [])

--instance FromJSON Permission where
--  parseJSON (Object v) = do
--    rsname <- v .: "rsname"
--    rsid <- v .: "rsid"
--    scopes <- fromMaybe [] <$> v .:? "scopes"
--    return $ Permission rsname rsid (map (\s -> Scope Nothing s) scopes)
--  parseJSON _          = mzero

data Owner = Owner {
  ownId   :: Maybe Text,
  ownName :: Text
  } deriving (Generic, Show)

instance FromJSON Owner where
  parseJSON = genericParseJSON $ aesonDrop 3 snakeCase 
instance ToJSON Owner where
  toJSON = genericToJSON $ (aesonDrop 3 snakeCase) {omitNothingFields = True}

data Resource = Resource {
     resId      :: Maybe ResourceId,
     resName    :: ResourceName,
     resType    :: Maybe Text,
     resUris    :: [Text],
     resScopes  :: [Scope],
     resOwner   :: Owner,
     resOwnerManagedAccess :: Bool,
     resAttributes :: [Attribute]
  } deriving (Generic, Show)

instance FromJSON Resource where
  parseJSON = genericParseJSON $ aesonDrop 3 camelCase 
instance ToJSON Resource where
  toJSON = genericToJSON $ (aesonDrop 3 camelCase) {omitNothingFields = True}

data Attribute = Attribute {
  attName   :: Text,
  attValues :: [Text]
  } deriving (Generic, Show)

instance FromJSON Attribute where
  parseJSON = genericParseJSON $ aesonDrop 3 camelCase 
instance ToJSON Attribute where
  toJSON = genericToJSON $ (aesonDrop 3 camelCase) {omitNothingFields = True}


data KCConfig = KCConfig {
  url :: Text,
  realm :: Text,
  clientId :: Text,
  clientSecret :: Text,
  adminLogin :: Text,
  adminPassword :: Text}

defaultConfig = KCConfig {
  url = "http://localhost:8080/auth",
  realm = "waziup",
  clientId = "api-server",
  clientSecret = "4e9dcb80-efcd-484c-b3d7-1e95a0096ac0",
  adminLogin = "cdupont",
  adminPassword = "password"}

