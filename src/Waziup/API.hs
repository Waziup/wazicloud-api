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

type API = ("api" :> "v1" :> WaziupAPI) :<|> WaziupDocs 

type WaziupAPI = AuthAPI
            :<|> SensorsAPI
            :<|> MeasurementsAPI
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

-- * Sensors

type SensorsAPI = Flat ( 
  "sensors" :> Header "Authorization" Token :> ( 
          QueryParam "q" SensorsQuery :> QueryParam "limit" SensorsLimit :> QueryParam "offset" SensorsOffset :> Get           '[JSON] [Sensor] 
    :<|>  ReqBody '[JSON] Sensor                                                                              :> PostNoContent '[JSON] NoContent
    :<|>  SensorAPI
    ))

type SensorAPI = Flat (
  Capture "sensor_id" SensorId :> (                         Get             '[JSON] Sensor
    :<|>                                                    DeleteNoContent '[JSON] NoContent
    :<|> "name"       :> ReqBody '[PlainText] SensorName :> PutNoContent    '[JSON] NoContent
    :<|> "location"   :> ReqBody '[JSON]      Location   :> PutNoContent    '[JSON] NoContent
    :<|> "gateway_id" :> ReqBody '[PlainText] GatewayId  :> PutNoContent    '[JSON] NoContent
    :<|> "visibility" :> ReqBody '[PlainText] Visibility :> PutNoContent    '[JSON] NoContent
    ))

type MeasurementsAPI = Flat (
  "sensors" :> Header "Authorization" Token :> Capture "sensor_id" SensorId :> 
  "measurements" :> (                   Get           '[JSON] [Measurement]
    :<|> ReqBody '[JSON] Measurement :> PostNoContent '[JSON] NoContent
    :<|> MeasurementAPI
    ))

type MeasurementAPI = Flat (
  Capture "meas_id" MeasId :> (                                    Get             '[JSON] Measurement
    :<|>                                                           DeleteNoContent '[JSON] NoContent
    :<|> "name"          :> ReqBody '[PlainText] MeasName       :> PutNoContent    '[JSON] NoContent
    :<|> "sensor_kind"   :> ReqBody '[PlainText] SensorKindId   :> PutNoContent    '[JSON] NoContent
    :<|> "quantity_kind" :> ReqBody '[PlainText] QuantityKindId :> PutNoContent    '[JSON] NoContent
    :<|> "unit"          :> ReqBody '[PlainText] UnitId         :> PutNoContent    '[JSON] NoContent
    :<|> "value"         :> ReqBody '[JSON] MeasurementValue    :> PostNoContent   '[JSON] NoContent
    ))

type SensorDataAPI = Flat (
  Header "Authorization" Token :> "sensors"      :> Capture "sensor_id" SensorId :> 
                                  "measurements" :> Capture "meas_id" MeasId :> 
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
  Capture "id" Text :> (                              Get             '[JSON] Project
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
