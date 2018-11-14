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

type WaziupAPI = "api" :> "v1" :> (AuthAPI :<|> SensorsAPI :<|> ProjectsAPI)

type AuthAPI = 
  "auth" :>  ("permissions" :> Header "Authorization" Token :> Get '[JSON] [Perm]
         :<|> "token"       :> ReqBody '[JSON] AuthBody :> Post '[PlainText] Token)

type SensorsAPI = Flat ( 
  "sensors" :> Header "Authorization" Token :> 
                (QueryParam "q" SensorsQuery :> QueryParam "limit" SensorsLimit :> QueryParam "offset" SensorsOffset :> Get '[JSON] [Sensor] :<|>
                 ReqBody '[JSON] Sensor :> PostNoContent '[JSON] NoContent :<|>
                 SensorAPI))

type SensorAPI = (
  Capture "id" Text :> (Get '[JSON] Sensor :<|>
                        DeleteNoContent '[JSON] NoContent))

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

