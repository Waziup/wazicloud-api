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

type ResourceId = Text
type ResourceName = Text
type ScopeId = Text
type ScopeName = Text
type Token = Text

data Scope = Scope {
  scpId   :: Maybe ScopeId,
  scpName :: ScopeName
  } deriving (Generic, Show)

instance FromJSON Scope where
  parseJSON = genericParseJSON $ aesonDrop 3 snakeCase 
instance ToJSON Scope where
  toJSON = genericToJSON $ aesonDrop 3 snakeCase

data Permission = Permission 
  { rsname :: ResourceName,
    rsid   :: ResourceId,
    scopes :: [Scope]
  } deriving (Generic, Show)

instance FromJSON Permission where
  parseJSON (Object v) = do
    rsname <- v .: "rsname"
    rsid <- v .: "rsid"
    scopes <- fromMaybe [] <$> v .:? "scopes"
    return $ Permission rsname rsid (map (\s -> Scope Nothing s) scopes)
  parseJSON _          = mzero

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
  realm :: Text,
  clientId :: Text,
  clientSecret :: Text,
  adminLogin :: Text,
  adminPassword :: Text}

defaultConfig = KCConfig { 
  realm = "waziup",
  clientId = "api-server",
  clientSecret = "4e9dcb80-efcd-484c-b3d7-1e95a0096ac0",
  adminLogin = "cdupont",
  adminPassword = "password"}

