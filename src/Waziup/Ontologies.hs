{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Ontologies where

import           Waziup.Utils
import           Waziup.Types
import           Control.Monad.Except (throwError)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.String.Conversions
import           Data.Aeson.BetterErrors as AB
import qualified Data.ByteString as BS
import           Servant
import           System.FilePath ((</>))
import           Paths_Waziup_Servant


-- * Ontologies

getSensingDevices :: Waziup [SensorKind]
getSensingDevices = do
  (WaziupInfo _ _ (Ontologies sks _ _)) <- ask
  return sks

getQuantityKinds :: Waziup [QuantityKind]
getQuantityKinds = do
  (WaziupInfo _ _ (Ontologies _ qks _)) <- ask
  return qks

getUnits :: Waziup [Unit]
getUnits = do
  (WaziupInfo _ _ (Ontologies _ _ us)) <- ask
  return us

