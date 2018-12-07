{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Waziup.API where

import Servant
import Servant.API.Flatten
import Waziup.Types
import Data.Text
import Data.Swagger hiding (Header)
import Keycloak
import Servant.Swagger.UI

-- | Waziup type-level API

type API = "api" :> "v2" :> WaziupAPI
      :<|> WaziupDocs 

type WaziupAPI = AuthAPI
            :<|> DevicesAPI
            :<|> SensorsAPI
            :<|> SensorDataAPI
            :<|> ProjectsAPI
            :<|> OntologiesAPI

type WaziupDocs = SwaggerSchemaUI "swagger-ui" "swagger.json"

-- * Authentication

type AuthAPI = 
  "auth" :>  (
         "permissions" :> Header "Authorization" Token :> Get  '[JSON]      [Perm]
    :<|> "token"       :> ReqBody '[JSON] AuthBody     :> Post '[PlainText] Token
    )

-- * Devices

type DevicesAPI = Flat ( 
  "devices" :> Header "Authorization" Token :> ( 
          QueryParam "q" DevicesQuery :> QueryParam "limit" DevicesLimit :> QueryParam "offset" DevicesOffset :> Get           '[JSON] [Device] 
    :<|>  ReqBody '[JSON] Device                                                                              :> PostNoContent '[JSON] NoContent
    :<|>  DeviceAPI
    ))

type DeviceAPI = Flat (
  Capture "device_id" DeviceId :> (                         Get             '[JSON] Device
    :<|>                                                    DeleteNoContent '[JSON] NoContent
    :<|> "name"       :> ReqBody '[PlainText] DeviceName :> PutNoContent    '[JSON] NoContent
    :<|> "location"   :> ReqBody '[JSON]      Location   :> PutNoContent    '[JSON] NoContent
    :<|> "gateway_id" :> ReqBody '[PlainText] GatewayId  :> PutNoContent    '[JSON] NoContent
    :<|> "visibility" :> ReqBody '[PlainText] Visibility :> PutNoContent    '[JSON] NoContent
    ))

type SensorsAPI = Flat (
  "devices" :> Header "Authorization" Token :> Capture "device_id" DeviceId :> 
  "snesors" :> (                        Get           '[JSON] [Sensor]
    :<|> ReqBody '[JSON] Sensor      :> PostNoContent '[JSON] NoContent
    :<|> SensorAPI
    ))

type SensorAPI = Flat (
  Capture "sensor_id" SensorId :> (                                Get             '[JSON] Sensor
    :<|>                                                           DeleteNoContent '[JSON] NoContent
    :<|> "name"          :> ReqBody '[PlainText] SensorName     :> PutNoContent    '[JSON] NoContent
    :<|> "sensor_kind"   :> ReqBody '[PlainText] SensorKindId   :> PutNoContent    '[JSON] NoContent
    :<|> "quantity_kind" :> ReqBody '[PlainText] QuantityKindId :> PutNoContent    '[JSON] NoContent
    :<|> "unit"          :> ReqBody '[PlainText] UnitId         :> PutNoContent    '[JSON] NoContent
    :<|> "value"         :> ReqBody '[JSON] SensorValue         :> PostNoContent   '[JSON] NoContent
    ))

type SensorDataAPI = Flat (
  Header "Authorization" Token :> "devices" :> Capture "device_id" DeviceId :> 
                                  "sensors" :> Capture "sensor_id" SensorId :> 
        Get '[JSON] [Datapoint])
    

-- * Projects

type ProjectsAPI = Flat ( 
  "projects" :> 
    Header "Authorization" Token :> (
                                     Get  '[JSON]      [Project]
    :<|>  ReqBody '[JSON] Project :> Post '[PlainText] ProjectId
    :<|>  ProjectAPI
    ))

type ProjectAPI = (
  Capture "id" ProjectId :> (                         Get             '[JSON] Project
    :<|>                                              DeleteNoContent '[JSON] NoContent
    :<|> "devices"  :> ReqBody '[JSON] [DeviceId]  :> PutNoContent    '[JSON] NoContent
    :<|> "gateways" :> ReqBody '[JSON] [GatewayId] :> PutNoContent    '[JSON] NoContent))

-- * Ontologies

type OntologiesAPI = "ontologies" :> ( "sensing_devices" :> Get '[JSON] [SensorKind] :<|>
                                       "quantity_kinds"  :> Get '[JSON] [QuantityKind] :<|>
                                       "units"           :> Get '[JSON] [Unit])


instance ToParamSchema Token where
  toParamSchema _ = binaryParamSchema
instance ToSchema Token where
  declareNamedSchema _ = pure (NamedSchema (Just "Token") binarySchema)
