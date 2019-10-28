{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Ontologies where

import           Waziup.Utils
import           Waziup.Types
import           Control.Monad.Except (throwError)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.String.Conversions
import qualified Data.ByteString as BS
import           Servant
import           System.FilePath ((</>))
import           Paths_Waziup_Servant


-- * Ontologies

getSensorKinds :: Waziup [SensorKind]
getSensorKinds = do
  (WaziupInfo _ _ (Ontologies sks _ _ _) _) <- ask
  return sks

getActuatorKinds :: Waziup [ActuatorKind]
getActuatorKinds = do
  (WaziupInfo _ _ (Ontologies _ acts _ _) _) <- ask
  return acts

getQuantityKinds :: Waziup [QuantityKind]
getQuantityKinds = do
  (WaziupInfo _ _ (Ontologies _ _ qks _) _) <- ask
  return qks

getUnits :: Waziup [Unit]
getUnits = do
  (WaziupInfo _ _ (Ontologies _ _ _ us) _) <- ask
  return us

