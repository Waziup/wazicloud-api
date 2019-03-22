{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Waziup.API where

import           Waziup.Types
import           Data.Text
import           Data.Time
import           Data.Aeson
import           Data.Swagger hiding (Header)
import qualified Data.Csv as CSV
import           Servant
import           Servant.API.Flatten
import           Servant.CSV.Cassava
import           Servant.Swagger.UI
import           Keycloak (Token, Username)
import           Orion

-----------------------------
-- | Waziup type-level API --
-----------------------------

-- Complete API
type API = "api" :> "v2" :> WaziupAPI
      :<|> WaziupDocs
      :<|> Redir 

-- Redirection for root requests
type Redir = (GetNoContent '[JSON] NoContent)

-- Waziup APIs
type WaziupAPI = AuthAPI
            :<|> DevicesAPI
            :<|> SensorsAPI
            :<|> ActuatorsAPI
            :<|> SensorDataAPI
            :<|> GatewaysAPI
            :<|> ProjectsAPI
            :<|> UsersAPI
            :<|> SocialsAPI
            :<|> NotifsAPI
            :<|> OntologiesAPI

-- Documentation
type WaziupDocs = SwaggerSchemaUI "docs" "swagger.json"

----------------------
-- * Authentication --
----------------------

type AuthAPI = 
  "auth" :>  (
         "permissions"
          :> Header "Authorization" Token
          :> Get  '[JSON]      [Perm]
    :<|> "token"
          :> ReqBody '[JSON] AuthBody
          :> Post '[PlainText] Token
    )


---------------
-- * Devices --
---------------

type DevicesAPI = Flat (Header "Authorization" Token :> 
  "devices" :> ( 
         QueryParam "q"      DevicesQuery
      :> QueryParam "limit"  Limit
      :> QueryParam "offset" Offset
      :> Get '[JSON] [Device] 
    :<|> ReqBody '[JSON] Device
      :> PostNoContent '[JSON] NoContent
    :<|> Capture "device_id" DeviceId :> (
            Get             '[JSON] Device
      :<|>  DeleteNoContent '[JSON] NoContent
      :<|> "name"       
           :> ReqBody '[PlainText] DeviceName
           :> PutNoContent    '[JSON] NoContent
      :<|> "location"
           :> ReqBody '[JSON] Location
           :> PutNoContent    '[JSON] NoContent
      :<|> "gateway_id"
           :> ReqBody '[PlainText] GatewayId
           :> PutNoContent    '[JSON] NoContent
      :<|> "visibility"
           :> ReqBody '[PlainText] Visibility
           :> PutNoContent    '[JSON] NoContent
      )))


---------------
-- * Sensors --
---------------

type SensorsAPI = Flat ( Header "Authorization" Token :> 
  "devices" :> Capture "device_id" DeviceId :> 
    "sensors" :> (
           Get '[JSON] [Sensor]
      :<|> ReqBody '[JSON] Sensor 
        :> PostNoContent '[JSON] NoContent
      :<|> Capture "sensor_id" SensorId :> (
             Get '[JSON] Sensor
        :<|> DeleteNoContent '[JSON] NoContent
        :<|> "name"          
           :> ReqBody '[PlainText] SensorName
           :> PutNoContent '[JSON] NoContent
        :<|> "sensor_kind"  
           :> ReqBody '[PlainText] SensorKindId
           :> PutNoContent '[JSON] NoContent
        :<|> "quantity_kind"
           :> ReqBody '[PlainText] QuantityKindId
           :> PutNoContent '[JSON] NoContent
        :<|> "unit"
           :> ReqBody '[PlainText] UnitId
           :> PutNoContent '[JSON] NoContent
        :<|> "calib"
           :> ReqBody '[JSON] Calib
           :> PutNoContent    '[JSON] NoContent
        :<|> "value"
           :> ReqBody '[JSON] SensorValue
           :> PostNoContent   '[JSON] NoContent
      )))


--------------------
-- * Sensors data --
--------------------

type SensorDataAPI = Flat (
  "sensors_data" :> Header "Authorization" Token 
  :> QueryParam "device_id" DeviceId
  :> QueryParam "sensor_id" SensorId
  :> QueryParam "limit"     Int
  :> QueryParam "offset"    Int
  :> QueryParam "sort"      Sort
  :> QueryParam "dateFrom"  UTCTime
  :> QueryParam "dataTo"    UTCTime
  :> Get '[JSON, CSV' 'HasHeader CSVOpts] [Datapoint])

data CSVOpts
instance EncodeOpts CSVOpts where
   encodeOpts _ = CSV.defaultEncodeOptions {CSV.encQuoting = CSV.QuoteNone}

-----------------
-- * Actuators --
-----------------

type ActuatorsAPI = Flat ( Header "Authorization" Token :> 
  "devices" :> Capture "device_id" DeviceId :> 
    "actuators" :> (
           Get '[JSON] [Actuator]
      :<|> ReqBody '[JSON] Actuator
        :> PostNoContent '[JSON] NoContent
      :<|> Capture "actuator_id" ActuatorId :> (
             Get '[JSON] Actuator 
        :<|> DeleteNoContent '[JSON] NoContent
        :<|> "name"
          :> ReqBody '[PlainText] ActuatorName
          :> PutNoContent '[JSON] NoContent
        :<|> "actuator_kind"
          :> ReqBody '[PlainText] ActuatorKindId
          :> PutNoContent '[JSON] NoContent
        :<|> "value_type"
          :> ReqBody '[PlainText] ActuatorValueTypeId
          :> PutNoContent '[JSON] NoContent
        :<|> "value"
          :> ReqBody '[JSON] Value
          :> PutNoContent '[JSON] NoContent
      )))


----------------
-- * Gateways --
----------------

type GatewaysAPI = Flat ( Header "Authorization" Token :> 
  "gateways" :> (
          Get  '[JSON] [Gateway]
    :<|>  ReqBody '[JSON] Gateway
       :> Post '[PlainText] GatewayId
    :<|> Capture "gw_id" GatewayId :> (
           Get '[JSON] Gateway
      :<|> DeleteNoContent '[JSON] NoContent
      :<|> "tunnel"
        :> ReqBody '[PlainText] Int
        :> PutNoContent '[JSON] NoContent
      :<|> "tunnel"
        :> DeleteNoContent '[JSON] NoContent
    )))


----------------
-- * Projects --
----------------

type ProjectsAPI = Flat ( Header "Authorization" Token :> 
  "projects" :> (
          Get  '[JSON]      [Project]
    :<|>  ReqBody '[JSON] Project
       :> Post '[PlainText] ProjectId
    :<|> Capture "id" ProjectId :> (
           Get '[JSON] Project
      :<|> DeleteNoContent '[JSON] NoContent
      :<|> "devices"
        :> ReqBody '[JSON] [DeviceId]
        :> PutNoContent '[JSON] NoContent
      :<|> "gateways"
        :> ReqBody '[JSON] [GatewayId]
        :> PutNoContent '[JSON] NoContent
    )))


-------------
-- * Users --
-------------

type UsersAPI = Flat ( Header "Authorization" Token :> 
  "users" :> (
         QueryParam "limit"  Limit
      :> QueryParam "offset" Offset
      :> QueryParam "username" Username
      :> Get '[JSON] [User]
    :<|> Capture "user_id" UserId
      :> Get '[JSON] User))


---------------
-- * Socials --
---------------

type SocialsAPI = Flat ( Header "Authorization" Token :> 
  "socials" :> (
          Get  '[JSON] [SocialMessage]
    :<|>  ReqBody '[JSON] SocialMessage
       :> Post '[PlainText] SocialMessageId
    :<|>  "batch"
       :> ReqBody '[JSON] SocialMessageBatch
       :> PostNoContent '[JSON] NoContent
    :<|> Capture "social_msg_id" SocialMessageId
       :> Get '[JSON] SocialMessage
    :<|> Capture "social_msg_id" SocialMessageId
       :> Delete '[JSON] NoContent
    ))


--------------------
-- * Notification --
--------------------

type NotifsAPI = Flat ( Header "Authorization" Token :> 
  "notifications" :> (
          Get  '[JSON] [Notif]
    :<|>  ReqBody '[JSON] Notif
       :> Post '[PlainText] NotifId
    :<|> Capture "notif_id" NotifId :> (
           Get '[JSON] Notif
      :<|> DeleteNoContent '[JSON] NoContent
      :<|> "status" 
       :> ReqBody '[PlainText] SubStatus 
       :> PutNoContent '[JSON] NoContent
    )))


------------------
-- * Ontologies --
------------------

type OntologiesAPI = 
  "ontologies" :> (
         "sensor_kinds"   :> Get '[JSON] [SensorKind]
    :<|> "actuator_kinds" :> Get '[JSON] [ActuatorKind]
    :<|> "quantity_kinds" :> Get '[JSON] [QuantityKind]
    :<|> "units"          :> Get '[JSON] [Unit])


instance ToParamSchema Token where
  toParamSchema _ = binaryParamSchema

instance ToSchema Token where
  declareNamedSchema _ = pure (NamedSchema (Just "Token") binarySchema)
