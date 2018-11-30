{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Ontologies where

import           Waziup.Utils
import           Waziup.Types
import           Control.Monad.Except (throwError)
import           Control.Monad.IO.Class
import           Data.String.Conversions
import           Data.Aeson.BetterErrors as AB
import qualified Data.ByteString as BS
import           Servant
import           System.FilePath ((</>))
import           Paths_Waziup_Servant


-- * Ontologies

getSensingDevices :: Waziup [SensingDeviceInfo]
getSensingDevices = do
  dir <- liftIO $ getDataDir
  msd <- liftIO $ BS.readFile $ dir </> "ontologies" </> "sensing_devices.json"
  case AB.parse (eachInArray parseSDI) (convertString msd) of
    Right sd -> return sd
    Left (e :: ParseError String) -> do
      error $ "Cannot decode data file: " ++ (show e)
      throwError err500 {errBody = "Cannot decode data file"}

getQuantityKinds :: Waziup [QuantityKindInfo]
getQuantityKinds = do
  dir <- liftIO getDataDir
  mqk <- liftIO $ BS.readFile $ dir </> "ontologies" </> "quantity_kinds.json"
  case AB.parse (eachInArray parseQKI) (convertString mqk) of
    Right qk -> return qk
    Left (e :: ParseError String) -> do
      error $ "Cannot decode data file: " ++ (show e)
      throwError err500 {errBody = "Cannot decode data file"}

getUnits :: Waziup [UnitInfo]
getUnits = do
  dir <- liftIO getDataDir
  mus <- liftIO $ BS.readFile $ dir </> "ontologies" </> "units.json"
  case AB.parse (eachInArray parseUnit) (convertString mus) of
    Right us -> return us
    Left (e :: ParseError String) -> do
      error $ "Cannot decode data file: " ++ (show e)
      throwError err500 {errBody = "Cannot decode data file"}

