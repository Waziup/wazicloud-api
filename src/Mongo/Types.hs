{-# LANGUAGE OverloadedStrings #-}

module Mongo.Types where

import Database.MongoDB as DB
import Data.Text

data MongoConfig = MongoConfig {
  mongoUrl :: Text } deriving (Show, Eq)

defaultMongoConfig = MongoConfig {
  mongoUrl = "mongodb://localhost:27017/"}
