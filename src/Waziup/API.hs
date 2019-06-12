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

type AuthAPI = Flat ( 
  "auth" :>  (
         "permissions" :> (
          "devices"
          :> Header "Authorization" Token
          :> Get  '[JSON]      [Perm]
          :<|> "projects"
          :> Header "Authorization" Token
          :> Get  '[JSON]      [Perm]
          :<|> "gateways"
          :> Header "Authorization" Token
          :> Get  '[JSON]      [Perm])
    :<|> "token"
          :> ReqBody '[JSON] AuthBody
          :> Post '[PlainText, JSON] Token
    ))


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
           :> ReqBody '[PlainText, JSON] DeviceName
           :> PutNoContent    '[JSON] NoContent
      :<|> "location"
           :> ReqBody '[JSON] Location
           :> PutNoContent    '[JSON] NoContent
      :<|> "gateway_id"
           :> ReqBody '[PlainText, JSON] GatewayId
           :> PutNoContent    '[JSON] NoContent
      :<|> "visibility"
           :> ReqBody '[PlainText, JSON] Visibility
           :> PutNoContent    '[JSON] NoContent
      :<|> "deployed"
           :> ReqBody '[PlainText, JSON] Bool
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
           :> ReqBody '[PlainText, JSON] SensorName
           :> PutNoContent '[JSON] NoContent
        :<|> "sensor_kind"  
           :> ReqBody '[PlainText, JSON] SensorKindId
           :> PutNoContent '[JSON] NoContent
        :<|> "quantity_kind"
           :> ReqBody '[PlainText, JSON] QuantityKindId
           :> PutNoContent '[JSON] NoContent
        :<|> "unit"
           :> ReqBody '[PlainText, JSON] UnitId
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
  :> QueryParam "device_id"  DeviceId
  :> QueryParam "sensor_id"  SensorId
  :> QueryParam "limit"      Int
  :> QueryParam "offset"     Int
  :> QueryParam "sort"       Sort
  :> QueryParam "date_from"  UTCTime
  :> QueryParam "date_to"    UTCTime
  :> QueryParam "calibrated" Bool
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
          :> ReqBody '[PlainText, JSON] ActuatorName
          :> PutNoContent '[JSON] NoContent
        :<|> "actuator_kind"
          :> ReqBody '[PlainText, JSON] ActuatorKindId
          :> PutNoContent '[JSON] NoContent
        :<|> "value_type"
          :> ReqBody '[PlainText, JSON] ActuatorValueTypeId
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
       :> Post '[PlainText, JSON] NoContent
    :<|> Capture "gw_id" GatewayId :> (
           Get '[JSON] Gateway
      :<|> DeleteNoContent '[JSON] NoContent
      :<|> "tunnel"
        :> ReqBody '[PlainText, JSON] Int
        :> PutNoContent '[JSON] NoContent
      :<|> "tunnel"
        :> DeleteNoContent '[JSON] NoContent
      :<|> "health"
        :> PutNoContent '[JSON] NoContent
    )))


----------------
-- * Projects --
----------------

type ProjectsAPI = Flat ( Header "Authorization" Token :> 
  "projects" :> (
          QueryParam "full" Bool
       :> Get '[JSON] [Project]
    :<|>  ReqBody '[JSON] Project
       :> Post '[PlainText, JSON] ProjectId
    :<|> Capture "id" ProjectId :> (
           QueryParam "full" Bool
        :> Get '[JSON] Project
      :<|> DeleteNoContent '[JSON] NoContent
      :<|> "devices"
        :> ReqBody '[JSON] [DeviceId]
        :> PutNoContent '[JSON] NoContent
      :<|> "gateways"
        :> ReqBody '[JSON] [GatewayId]
        :> PutNoContent '[JSON] NoContent
      :<|> "name"
         :> ReqBody '[JSON] Text 
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
    :<|> ReqBody '[JSON] User
      :> Post '[JSON] UserId
    :<|> Capture "user_id" UserId :> (
           Get '[JSON] User
      :<|> "sms_credit" 
        :> ReqBody '[JSON] Int
        :> PutNoContent '[JSON] NoContent)))


---------------
-- * Socials --
---------------

type SocialsAPI = Flat ( Header "Authorization" Token :> 
  "socials" :> (
          Get  '[JSON] [SocialMessage]
    :<|>  ReqBody '[JSON] SocialMessage
       :> Post '[PlainText, JSON] SocialMessageId
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
       :> Post '[PlainText, JSON] NotifId
    :<|> Capture "notif_id" NotifId :> (
           Get '[JSON] Notif
      :<|> DeleteNoContent '[JSON] NoContent
      :<|> "status" 
       :> ReqBody '[PlainText, JSON] SubStatus 
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
