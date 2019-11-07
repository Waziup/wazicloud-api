{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Ontologies where

import           Waziup.Types
import           Control.Monad.Reader


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

