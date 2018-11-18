{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Waziup.API where

import Servant
import Servant.API.Flatten
import Waziup.Types
import Data.Text
import Keycloak


-- | Waziup type-level API

type WaziupAPI = "api" :> "v1" :> (AuthAPI :<|> SensorsAPI :<|> ProjectsAPI :<|> OntologiesAPI)


-- * Authentication

type AuthAPI = 
  "auth" :>  ("permissions" :> Header "Authorization" Token :> Get '[JSON] [Perm]
         :<|> "token"       :> ReqBody '[JSON] AuthBody :> Post '[PlainText] Token)

-- * Sensors

type SensorsAPI = Flat ( 
  "sensors" :> Header "Authorization" Token :> 
                (QueryParam "q" SensorsQuery :> QueryParam "limit" SensorsLimit :> QueryParam "offset" SensorsOffset :> Get '[JSON] [Sensor] :<|>
                 ReqBody '[JSON] Sensor :> PostNoContent '[JSON] NoContent :<|>
                 SensorAPI))

type SensorAPI = (
  Capture "id" Text :> (Get '[JSON] Sensor :<|>
                        DeleteNoContent '[JSON] NoContent))

-- * Projects

type ProjectsAPI = Flat ( 
  "projects" :> Header "Authorization" Token :> 
                (Get '[JSON] [Project] :<|>
                 ReqBody '[JSON] Project :> Post '[PlainText] ProjectId :<|>
                 ProjectAPI))

type ProjectAPI = (
  Capture "id" Text :> (Get '[JSON] Project :<|>
                        DeleteNoContent '[JSON] NoContent :<|>
                        "devices" :> ReqBody '[JSON] [DeviceId] :> PutNoContent '[JSON] NoContent :<|>
                        "gateways" :> ReqBody '[JSON] [GatewayId] :> PutNoContent '[JSON] NoContent))

-- * Ontologies

type OntologiesAPI = "ontologies" :> ( "sensing_devices" :> Get '[JSON] [SensingDeviceInfo] :<|>
                                       "quantity_kinds"  :> Get '[JSON] [QuantityKindInfo] :<|>
                                       "units"           :> Get '[JSON] [UnitInfo])

