{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Ontologies where

import           Waziup.Types
import           Control.Monad.Reader
import           Control.Lens

-- * Ontologies

getSensorKinds :: Waziup [SensorKind]
getSensorKinds = view $ ontologies.sensingDevices

getActuatorKinds :: Waziup [ActuatorKind]
getActuatorKinds = view $ ontologies.actuatingDevices

getQuantityKinds :: Waziup [QuantityKind]
getQuantityKinds = view $ ontologies.quantityKinds

getUnits :: Waziup [Unit]
getUnits = view $ ontologies.units

