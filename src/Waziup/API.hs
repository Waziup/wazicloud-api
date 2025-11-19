{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Waziup.API where

import           Waziup.Types
import qualified Data.ByteString.Lazy as L
import           Data.Text
import           Data.Time
import           Data.Aeson
import           Data.Swagger hiding (Header)
import qualified Data.Csv as CSV
import           Servant
import           Servant.API.Flatten
import           Servant.CSV.Cassava
import           Servant.Swagger.UI
import           Servant.Swagger.Tags
import           Servant.Auth.Server
import qualified Keycloak as KC
import           Keycloak (Username)
import           Orion

-----------------------------
-- | Waziup type-level API --
-----------------------------

-- Complete API
type API = "api" :> "v2" :> WaziupAPI
      :<|> DocsAPI
      :<|> Redir

-- Redirection for root requests
type Redir = (GetNoContent '[JSON] NoContent)

-- Token API and resources API
type WaziupAPI = Auth '[JWT] User :> ResourcesAPI
            :<|> PostAuthAPI

type PostAuthAPI = "auth" :> "token"
                  :> Tags "Auth" 
                  :> ReqBody '[JSON] AuthBody
                  :> Post '[PlainText, JSON] Token

-- All resources APIs
type ResourcesAPI =
            Tags "Auth"       :> AuthAPI
       :<|> Tags "Devices"    :> DevicesAPI
       :<|> Tags "Sensors"   :> SensorsAPI
       :<|> Tags "Actuators"  :> ActuatorsAPI
       :<|> Tags "Datapoints" :> SensorDataAPI
       :<|> Tags "Gateways"   :> GatewaysAPI
       :<|> Tags "Projects"   :> ProjectsAPI
       :<|> Tags "Users"      :> UsersAPI
       :<|> Tags "Socials"    :> SocialsAPI
       :<|> Tags "Notifs"     :> NotifsAPI
       :<|> Tags "Ontologies" :> OntologiesAPI

-- Documentation
type DocsAPI = SwaggerSchemaUI "docs" "swagger.json"

----------------------
-- * Authentication --
----------------------

type AuthAPI = "auth" :> "permissions" :> (
               "devices"  :> Get '[JSON] [Perm]
          :<|> "projects" :> Get '[JSON] [Perm]
          :<|> "gateways" :> Get '[JSON] [Perm])


---------------
-- * Devices --
---------------

type DevicesAPI = Flat (
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
      :<|> "meta"       
           :> ReqBody '[JSON] MetadataValue
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
      :<|> "owner"
           :> ReqBody '[PlainText, JSON] Username
           :> PutNoContent    '[JSON] NoContent
      )))


---------------
-- * Sensors --
---------------

type SensorsAPI = Flat (
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
        :<|> "meta"          
           :> ReqBody '[JSON] MetadataValue
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
           :> ReqBody '[JSON, PlainText] SensorValue
           :> PostNoContent   '[JSON] NoContent
        :<|> "values"
           :> ReqBody '[JSON] [SensorValue]
           :> PostNoContent   '[JSON] NoContent
        :<|> "values"
           :> QueryParam "limit"      Int
           :> QueryParam "offset"     Int
           :> QueryParam "sort"       Sort
           :> QueryParam "date_from"  UTCTime
           :> QueryParam "date_to"    UTCTime
           :> QueryParam "calibrated" Bool
           :> Get '[JSON, CSV' 'HasHeader CSVOpts] [Datapoint]
      )))


--------------------
-- * Sensors data --
--------------------

type SensorDataAPI = 
  "sensors_data"
  :> QueryParam "device_id"  [DeviceId]
  :> QueryParam "sensor_id"  [SensorId]
  :> QueryParam "limit"      Int
  :> QueryParam "offset"     Int
  :> QueryParam "sort"       Sort
  :> QueryParam "date_from"  UTCTime
  :> QueryParam "date_to"    UTCTime
  :> QueryParam "calibrated" Bool
  :> Get '[JSON, CSV' 'HasHeader CSVOpts] [Datapoint]

data CSVOpts
instance EncodeOpts CSVOpts where
   encodeOpts _ = CSV.defaultEncodeOptions {CSV.encQuoting = CSV.QuoteNone}

-----------------
-- * Actuators --
-----------------

type ActuatorsAPI = Flat (
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
        :<|> "meta"          
           :> ReqBody '[JSON] MetadataValue
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

type GatewaysAPI = Flat (
  "gateways" :> (
          QueryParam "full" Bool
       :> Get  '[JSON] [Gateway]
    :<|>  ReqBody '[JSON] Gateway
       :> Post '[PlainText, JSON] NoContent
    :<|> Capture "gw_id" GatewayId :> (
           QueryParam "full" Bool
        :> Get '[JSON] Gateway
      :<|> DeleteNoContent '[JSON] NoContent
      :<|> "heartbeat"
        :> PutNoContent '[JSON] NoContent
      :<|> "name"       
           :> ReqBody '[PlainText, JSON] GatewayName
           :> PutNoContent    '[JSON] NoContent
      :<|> "owner"       
           :> ReqBody '[PlainText, JSON] Username
           :> PutNoContent    '[JSON] NoContent
      :<|> "location"       
           :> ReqBody '[JSON] Location
           :> PutNoContent    '[JSON] NoContent
      :<|> "vpn"       
           :> Get '[JSON] VPNFile
    )))


----------------
-- * Projects --
----------------

type ProjectsAPI = Flat (
  "projects" :> (
          QueryParam "full" Bool
       :> Get '[JSON] [Project]
    :<|>  ReqBody '[JSON] Project
       :> Post '[PlainText, JSON] ProjectId
    :<|> Capture "id" ProjectId :> (
           QueryParam "full" Bool
        :> Get '[JSON] Project
      :<|> DeleteNoContent '[JSON] NoContent
      :<|> "device_ids"
        :> ReqBody '[JSON] [DeviceId]
        :> PutNoContent '[JSON] NoContent
      :<|> "gateway_ids"
        :> ReqBody '[JSON] [GatewayId]
        :> PutNoContent '[JSON] NoContent
      :<|> "name"
         :> ReqBody '[JSON] Text 
         :> PutNoContent '[JSON] NoContent
    )))


-------------
-- * Users --
-------------

type UsersAPI = Flat (
  "users" :> (
         QueryParam "limit"  Limit
      :> QueryParam "offset" Offset
      :> QueryParam "username" Username
      :> Get '[JSON] [User]
    :<|> ReqBody '[JSON] User
      :> Post '[JSON] UserId
    :<|> "me" :> Get '[JSON] User
    :<|> Capture "user_id" UserId :> (
           Get '[JSON] User
      :<|> "sms_credit" 
        :> ReqBody '[JSON] Int
        :> PutNoContent '[JSON] NoContent)))


---------------
-- * Socials --
---------------

type SocialsAPI = Flat (
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

type NotifsAPI = Flat (
  "notifications" :> (
          Get  '[JSON] [Notif]
    :<|>  ReqBody '[JSON] Notif
       :> Post '[PlainText, JSON] NotifId
    :<|> Capture "notif_id" NotifId :> (
           Get '[JSON] Notif
      :<|> ReqBody '[JSON] Notif
       :> Patch '[JSON] NoContent
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


