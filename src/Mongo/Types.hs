{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Mongo.Types where

import Database.MongoDB as DB
import Data.Text
import Control.Lens

data MongoConfig = MongoConfig {
  _mongoUrl :: Text } deriving (Show, Eq)

defaultMongoConfig = MongoConfig {
  _mongoUrl = "127.0.0.1"}

makeLenses ''MongoConfig
