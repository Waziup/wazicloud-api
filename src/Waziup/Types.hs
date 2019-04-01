{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Waziup.Types where

import           Data.List (stripPrefix)
import           Data.Maybe (fromMaybe)
import           Data.Aeson as Aeson
import           Data.Aeson.Types as AT (Options(..), defaultOptions, Pair)
import           Data.Aeson.Casing (snakeCase, aesonDrop)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HM
import           Data.Function ((&))
import           Data.Time
import           Data.Time.ISO8601
import           Data.Maybe
import           Data.Char
import           Data.Monoid
import           Data.Time.ISO8601
import           Data.Swagger hiding (fieldLabelModifier)
import qualified Data.Swagger as SW
import           Data.Swagger.Internal
import           Data.Swagger.Lens
import           Data.String.Conversions
import           Data.Pool
import qualified Data.Csv as CSV
import           Control.Lens hiding ((.=))
import           Control.Monad
import           Control.Monad.Except (ExceptT, throwError, runExceptT)
import           Control.Monad.Catch as C
import           Control.Monad.Reader
import           Servant
import           Servant.Swagger
import           Servant.API.Flatten
import           Keycloak as KC hiding (info, warn, debug, Scope, User(..), UserId, unCapitalize) 
import           GHC.Generics (Generic)
import qualified Database.MongoDB as DB
import qualified Orion.Types as O
import           Web.Twitter.Conduit hiding (map)
import           Debug.Trace
import           Safe

type Limit  = Int
type Offset = Int

--------------------
-- * Waziup Monad --
--------------------

-- Monad for running requests against Waziup backend components
-- Servant.Handler is ExceptT ServantErr IO a
type Waziup = ReaderT WaziupInfo Servant.Handler

data WaziupInfo = WaziupInfo {
  _dbPool       :: Pool DB.Pipe,
  _waziupConfig :: WaziupConfig,
  _ontologies   :: Ontologies
  }

-- | run a Waziup monad
runWaziup :: Waziup a -> WaziupInfo -> IO (Either ServantErr a)
runWaziup w wi = runExceptT $ runHandler' $ runReaderT w wi

--------------
-- * Config --
--------------

data WaziupConfig = WaziupConfig {
  _serverConf    :: ServerConfig,
  _mongoConf     :: MongoConfig,
  _keycloakConf  :: KCConfig,
  _orionConf     :: O.OrionConfig,
  _mqttConf      :: MQTTConfig,
  _twitterConf   :: TWInfo,
  _plivoConf     :: PlivoConfig
  } deriving (Eq, Show)

-- | Server or client configuration, specifying the host and port to query or serve on.
data ServerConfig = ServerConfig {
  _serverHost     :: Text,     -- ^ Hostname to serve on, e.g. "127.0.0.1"
  _serverPort     :: Int,      -- ^ Port to serve on, e.g. 8080
  _serverPortMQTT :: Int,      -- ^ Port to serve on, e.g. 2883 
  _guestLogin     :: Username,
  _guestPassword  :: Password} deriving (Eq, Show)

defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig {
  _serverHost     = "http://localhost:3000",
  _serverPort     = 3000,
  _serverPortMQTT = 3883,
  _guestLogin     = "guest",
  _guestPassword  = "guest"}
 
data MongoConfig = MongoConfig {
  _mongoUrl :: Text } deriving (Show, Eq)

defaultMongoConfig = MongoConfig {
  _mongoUrl = "127.0.0.1"}

data MQTTConfig = MQTTConfig {
  _mqttHost :: Text,
  _mqttPort :: Int } deriving (Show, Eq)

defaultMQTTConfig = MQTTConfig {
  _mqttHost = "127.0.0.1",
  _mqttPort = 1883 }

defaultTwitterConf :: TWInfo
defaultTwitterConf = 
  def { twProxy = Nothing,
        twToken = def { 
          twOAuth = twitterOAuth { oauthConsumerKey = "<Your consumer key>", 
                                   oauthConsumerSecret = "<Your consumer secret>"}, 
          twCredential = Credential [ ("oauth_token", "<Your OAuth token>"), 
                                      ("oauth_token_secret", "<Your OAuth token secret>")]}}

data PlivoConfig = PlivoConfig {
  _plivoHost  :: Text,
  _plivoID    :: Text,
  _plivoToken :: Text} deriving (Show, Eq)

defaultPlivoConf = PlivoConfig {
  _plivoHost  = "https://api.plivo.com",
  _plivoID    = "<Your Plivo ID>",
  _plivoToken = "<Your Plivo token>"}

--------------------------------------
-- * Authentication & authorization --
--------------------------------------

data AuthBody = AuthBody
  { authBodyUsername :: Username
  , authBodyPassword :: Password
  } deriving (Show, Eq, Generic)

instance ToJSON AuthBody where
  toJSON = genericToJSON $ snakeDrop 8

instance FromJSON AuthBody where
  parseJSON = genericParseJSON $ snakeDrop 8

instance ToSchema AuthBody where
   declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions {SW.fieldLabelModifier = snakeCase . drop 8} proxy
        & mapped.schema.example ?~ toJSON (AuthBody "cdupont" "password")

-- | Permission
data Perm = Perm
  { permResource :: Text -- ^ 
  , permScopes :: [Scope] -- ^ 
  } deriving (Show, Eq, Generic)

instance ToJSON Perm where
  toJSON = genericToJSON $ snakeDrop 4

instance ToSchema Perm where
   declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions {SW.fieldLabelModifier = snakeCase . drop 4} proxy

data Scope = DevicesCreate
           | DevicesUpdate
           | DevicesView
           | DevicesDelete
           | DevicesDataCreate
           | DevicesDataView
   deriving (Show, Read, Eq, Generic)

instance ToJSON Scope where
  toJSON = genericToJSON defaultOptions {AT.constructorTagModifier = fromScope'}

instance FromJSON Scope where
  parseJSON = genericParseJSON defaultOptions {AT.constructorTagModifier = toScope'}

instance ToSchema Scope where
   declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions {SW.constructorTagModifier = fromScope'} proxy

toScope :: Text -> Maybe Scope
toScope "devices:create"      = Just DevicesCreate    
toScope "devices:update"      = Just DevicesUpdate    
toScope "devices:view"        = Just DevicesView      
toScope "devices:delete"      = Just DevicesDelete    
toScope "devices-data:create" = Just DevicesDataCreate
toScope "devices-data:view"   = Just DevicesDataView  
toScope _                     = Nothing

-- Convert KC representation to Scope , e.g. "devices:create" to "DeviceCreate"
toScope' :: String -> String
toScope' a = case toScope $ convertString a of
  Just s -> show s
  Nothing -> a

fromScope :: Scope -> Text
fromScope DevicesCreate     = "devices:create"       
fromScope DevicesUpdate     = "devices:update"       
fromScope DevicesView       = "devices:view"         
fromScope DevicesDelete     = "devices:delete"       
fromScope DevicesDataCreate = "devices-data:create"  
fromScope DevicesDataView   = "devices-data:view"   

-- Convert Scope to KC representation, e.g. "DeviceCreate" to "devices:create"
fromScope' :: String -> String
fromScope' a = case readMay a of
  Just s -> convertString $ fromScope s
  Nothing -> a


---------------
-- * Devices --
---------------

type DeviceName    = Text
type Domain        = Text
type DevicesQuery  = Text

-- Id of a device
newtype DeviceId = DeviceId {unDeviceId :: Text} deriving (Show, Eq, Generic)

-- JSON instances
instance ToJSON DeviceId where
  toJSON = genericToJSON (defaultOptions {AT.unwrapUnaryRecords = True})

instance FromJSON DeviceId where
  parseJSON = genericParseJSON (defaultOptions {AT.unwrapUnaryRecords = True})

-- DeviceId is used in Url pieces
instance FromHttpApiData DeviceId where
  parseUrlPiece a = Right $ DeviceId a 

-- Rendering in Swagger
instance ToSchema DeviceId where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions {SW.unwrapUnaryRecords = True} proxy
        & mapped.schema.example ?~ toJSON (DeviceId "MyDevice")

instance ToParamSchema DeviceId

-- | one device 
data Device = Device
  { devId           :: DeviceId         -- ^ Unique ID of the device node
  , devGatewayId    :: Maybe GatewayId  -- ^ Unique ID of the gateway
  , devName         :: Maybe DeviceName -- ^ name of the device node
  , devLocation     :: Maybe Location
  , devDomain       :: Maybe Domain     -- ^ the domain of this device.
  , devVisibility   :: Maybe Visibility
  , devSensors      :: [Sensor]
  , devActuators    :: [Actuator]
  , devOwner        :: Maybe Username   -- ^ owner of the device node (output only)
  , devDateCreated  :: Maybe UTCTime    -- ^ creation date of the device (output only)
  , devDateModified :: Maybe UTCTime    -- ^ last update date of the device node (output only)
  , devKeycloakId   :: Maybe ResourceId -- ^ The is of the resource in Keycloak
  } deriving (Show, Eq, Generic)

defaultDevice = Device
  { devId           = DeviceId "MyDevice"
  , devGatewayId    = Just $ GatewayId "MyGW" 
  , devName         = Just "My weather station" 
  , devLocation     = Just defaultLocation 
  , devDomain       = Just "waziup" 
  , devVisibility   = Just Public
  , devSensors      = [defaultSensor]
  , devActuators    = [defaultActuator]
  , devOwner        = Nothing
  , devDateCreated  = Nothing
  , devDateModified = Nothing
  , devKeycloakId   = Nothing 
  }

instance ToJSON Device where
  toJSON = genericToJSON $ snakeDrop 3

instance FromJSON Device where
  parseJSON = genericParseJSON $ snakeDrop 3

instance ToSchema Device where
  declareNamedSchema proxy = genericDeclareNamedSchema (defaultSchemaOptions {SW.fieldLabelModifier = snakeCase . drop 3}) proxy
        & mapped.schema.example ?~ toJSON defaultDevice 


-- * Visibility

data Visibility = Public | Private
  deriving (Show, Eq, Generic)

--JSON instances
instance ToJSON Visibility where
  toJSON = genericToJSON $ defaultOptions {AT.constructorTagModifier = unCapitalize, AT.allNullaryToStringTag = True}

instance FromJSON Visibility where
  parseJSON = genericParseJSON $ defaultOptions {AT.constructorTagModifier = unCapitalize, AT.allNullaryToStringTag = True}

-- Visibility is use as plain text body
instance MimeUnrender PlainText Visibility where
  mimeUnrender _ "public"  = Right Public
  mimeUnrender _ "private" = Right Private
  mimeUnrender _ _         = Left "Cannot decode visibility. Valid values are \"public\" and \"private\"."

--Swagger instances
instance ToParamSchema Visibility

instance ToSchema Visibility where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions {SW.constructorTagModifier = unCapitalize} proxy
        & mapped.schema.example ?~ "public" 

toVisibility :: Text -> Maybe Visibility
toVisibility "public" = Just Public
toVisibility "private" = Just Private
toVisibility _ = Nothing

fromVisibility :: Visibility -> Text
fromVisibility Public  = "public"
fromVisibility Private = "private"

-- * Location

newtype Latitude  = Latitude Double deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance ToSchema Longitude

newtype Longitude = Longitude Double deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance ToSchema Latitude

-- | location is a pair [latitude, longitude] with the coordinates on earth in decimal notation (e.g. [40.418889, 35.89389]).
data Location = Location
  { latitude  :: Latitude
  , longitude :: Longitude
  } deriving (Show, Eq, Generic)

defaultLocation :: Location
defaultLocation = Location (Latitude 5.36) (Longitude 4.0083)

--JSON instances
instance FromJSON Location where
  parseJSON = genericParseJSON defaultOptions

instance ToJSON Location where
  toJSON = genericToJSON defaultOptions

--Swagger instance
instance ToSchema Location where
   declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.example ?~ toJSON defaultLocation 


---------------
-- * Sensors --
---------------

type SensorName = Text

-- Id of a sensor
newtype SensorId = SensorId {unSensorId :: Text} deriving (Show, Eq, Generic)

-- JSON instances
instance ToJSON SensorId where
  toJSON = genericToJSON (defaultOptions {AT.unwrapUnaryRecords = True})

instance FromJSON SensorId where
  parseJSON = genericParseJSON (defaultOptions {AT.unwrapUnaryRecords = True})

-- SensorId is used in Url pieces
instance FromHttpApiData SensorId where
  parseUrlPiece a = Right $ SensorId a 

--Swagger instances
instance ToSchema SensorId where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions {SW.unwrapUnaryRecords = True} proxy
        & mapped.schema.example ?~ toJSON (SensorId "TC") 

instance ToParamSchema SensorId


-- | one sensor 
data Sensor = Sensor
  { senId            :: SensorId               -- ^ ID of the sensor
  , senName          :: Maybe SensorName       -- ^ name of the sensor
  , senSensorKind    :: Maybe SensorKindId     -- ^ sensing platform used for the sensor, from https://github.com/Waziup/waziup-js/blob/master/src/model/SensingDevices.js
  , senQuantityKind  :: Maybe QuantityKindId   -- ^ quantity measured, from https://github.com/Waziup/waziup-js/blob/master/src/model/QuantityKinds.js
  , senUnit          :: Maybe UnitId           -- ^ unit of the measurement, from https://github.com/Waziup/waziup-js/blob/master/src/model/Units.js
  , senValue         :: Maybe SensorValue      -- ^ last value received by the platform
  , senCalib         :: Maybe Calib
  } deriving (Show, Eq, Generic)

defaultSensor = Sensor
  { senId            = SensorId "TC1" 
  , senName          = Just "My garden temperature" 
  , senSensorKind    = Just $ SensorKindId "Thermometer" 
  , senQuantityKind  = Just $ QuantityKindId "AirTemperature" 
  , senUnit          = Just $ UnitId "DegreeCelsius"
  , senValue         = Nothing
  , senCalib         = Nothing
  } 

instance FromJSON Sensor where
  parseJSON = genericParseJSON $ snakeDrop 3

instance ToJSON Sensor where
  toJSON = genericToJSON $ snakeDrop 3

instance ToSchema Sensor where
   declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions {SW.fieldLabelModifier = snakeCase . drop 3} proxy
        & mapped.schema.example ?~ toJSON defaultSensor 

-- * sensor value 

data SensorValue = SensorValue
  { senValValue        :: Value          -- ^ value of the measurement
  , senValTimestamp    :: Maybe UTCTime  -- ^ time of the measurement
  , senValDateReceived :: Maybe UTCTime  -- ^ time at which the measurement has been received on the Cloud
  } deriving (Show, Eq, Generic)

defaultSensorValue = SensorValue 
  { senValValue        = Number 25
  , senValTimestamp    = parseISO8601 "2016-06-08T18:20:27.873Z"
  , senValDateReceived = Nothing
  }

--JSON instances
instance FromJSON SensorValue where
  parseJSON = genericParseJSON $ snakeDrop 6

instance ToJSON SensorValue where
  toJSON = genericToJSON $ snakeDrop 6

--Swagger instance
instance ToSchema SensorValue where
   declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions {SW.fieldLabelModifier = snakeCase . drop 6} proxy
        & mapped.schema.example ?~ toJSON defaultSensorValue 

--Swagger instance for any JSON value
instance ToSchema Value where
  declareNamedSchema _ = pure (NamedSchema (Just "Value") (mempty & type_ .~ SwaggerObject))


-- * Calibration

data Calib = Linear CalibLinear | Function CalibFunction deriving (Show, Eq, Generic)

instance FromJSON Calib where
  parseJSON = genericParseJSON $ defaultOptions {AT.constructorTagModifier = unCapitalize, sumEncoding = ObjectWithSingleField} 

instance ToJSON Calib where
  toJSON = genericToJSON $ defaultOptions {AT.constructorTagModifier = unCapitalize, sumEncoding = ObjectWithSingleField} 

--Swagger instance
instance ToSchema Calib where
   declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions {SW.constructorTagModifier = unCapitalize} proxy
        & mapped.schema.example ?~ (toJSON $ Linear defaultCalibLinear)

data CalibLinear = CalibLinear 
  { calEnabled  :: Bool
  , calValueMin :: CalibValue
  , calValueMax :: CalibValue
  } deriving (Show, Eq, Generic)

defaultCalibLinear = CalibLinear
  { calEnabled  = True
  , calValueMin = CalibValue 900 100
  , calValueMax = CalibValue 300 0
  }

--JSON instances
instance FromJSON CalibLinear where
  parseJSON = genericParseJSON $ snakeDrop 3

instance ToJSON CalibLinear where
  toJSON = genericToJSON $ snakeDrop 3

--Swagger instance
instance ToSchema CalibLinear where
   declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions {SW.fieldLabelModifier = snakeCase . drop 3} proxy
        & mapped.schema.example ?~ toJSON defaultCalibLinear


data CalibValue = CalibValue
  { calSensorValue :: Double 
  , calRealValue   :: Double
  } deriving (Show, Eq, Generic)

--JSON instances
instance FromJSON CalibValue where
  parseJSON = genericParseJSON $ snakeDrop 3

instance ToJSON CalibValue where
  toJSON = genericToJSON $ snakeDrop 3

instance ToSchema CalibValue

data CalibFunction = FunctionCalib
  { calFuncEnabled :: Bool
  , calFuncFunc    :: Text
  } deriving (Show, Eq, Generic) 

--JSON instances
instance FromJSON CalibFunction where
  parseJSON = genericParseJSON $ snakeDrop 7

instance ToJSON CalibFunction where
  toJSON = genericToJSON $ snakeDrop 7

instance ToSchema CalibFunction where
   declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions {SW.fieldLabelModifier = snakeCase . drop 7} proxy
        & mapped.schema.example ?~ ""

-------------------
-- * Data points --
-------------------

data CalibEn = CalibCalib | CalibRaw

-- | one datapoint 
data Datapoint = Datapoint
  { dataDeviceId     :: DeviceId       -- ^ ID of the device
  , dataSensorId     :: SensorId       -- ^ ID of the sensor
  , dataValue        :: Value          -- ^ value of the measurement
  , dataTimestamp    :: Maybe UTCTime  -- ^ time of the measurement
  , dataDateReceived :: Maybe UTCTime  -- ^ time at which the measurement has been received on the Cloud
  } deriving (Show, Eq, Generic)

defaultDatapoint = Datapoint 
  { dataDeviceId     = DeviceId "MyDevice90"   -- ^ ID of the device
  , dataSensorId     = SensorId "TC1"          -- ^ ID of the sensor
  , dataValue        = Number 25
  , dataTimestamp    = parseISO8601 "2016-06-08T18:20:27.873Z"
  , dataDateReceived = Nothing
  }

-- JSON instances
instance FromJSON Datapoint where
  parseJSON = genericParseJSON $ snakeDrop 4

instance ToJSON Datapoint where
  toJSON = genericToJSON $ snakeDrop 4

-- CSV instances
instance CSV.ToField DeviceId where
  toField (DeviceId d) = convertString d

instance CSV.ToField SensorId where
  toField (SensorId d) = convertString d

instance CSV.ToField Value where 
  toField v = convertString $ encode v

instance CSV.ToField UTCTime where 
  toField d = convertString $ formatISO8601 d

myOptions :: CSV.Options
myOptions = CSV.defaultOptions { CSV.fieldLabelModifier = snakeCase . drop 4 }

instance CSV.DefaultOrdered Datapoint where
  headerOrder = CSV.genericHeaderOrder myOptions

instance CSV.ToNamedRecord Datapoint where
  toNamedRecord = CSV.genericToNamedRecord myOptions 

-- Swagger instance
instance ToSchema Datapoint where
   declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.example ?~ toJSON defaultDatapoint 


-- * Query options

data Sort = Asc | Dsc
  deriving (Eq, Generic)

instance ToJSON Sort where
  toJSON = genericToJSON $ defaultOptions {fieldLabelModifier = unCapitalize, AT.allNullaryToStringTag = True}

instance FromJSON Sort where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = unCapitalize, AT.allNullaryToStringTag = True}

-- Sort is used in Url pieces
instance FromHttpApiData Sort where
  parseUrlPiece "asc" = Right Asc
  parseUrlPiece "dsc" = Right Dsc
  parseUrlPiece _ = Left "Wrong sort parameter. Acceptable are \"asc\" or \"dsc\""

instance ToParamSchema Sort where
  toParamSchema _ = mempty
     & type_ .~ SwaggerString
     & enum_ ?~ [ "asc", "dsc" ]


-----------------
-- * Actuators --
-----------------

type ActuatorName = Text

-- Id of an actuator
newtype ActuatorId = ActuatorId {unActuatorId :: Text} deriving (Show, Eq, Generic)

-- JSON instances
instance ToJSON ActuatorId where
  toJSON = genericToJSON (defaultOptions {AT.unwrapUnaryRecords = True})

instance FromJSON ActuatorId where
  parseJSON = genericParseJSON (defaultOptions {AT.unwrapUnaryRecords = True})

-- SensorId is used in Url pieces
instance FromHttpApiData ActuatorId where
  parseUrlPiece a = Right $ ActuatorId a 

--Swagger instances
instance ToSchema ActuatorId where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.example ?~ toJSON (SensorId "Act1") 

instance ToParamSchema ActuatorId


-- | one actuator 
data Actuator = Actuator
  { actId                 :: ActuatorId         -- ^ ID of the actuator
  , actName               :: Maybe ActuatorName -- ^ name of the actuator
  , actActuatorKind       :: Maybe ActuatorKindId
  , actActuatorValueType  :: Maybe ActuatorValueTypeId
  , actValue              :: Maybe Value
  } deriving (Show, Eq, Generic)

defaultActuator = Actuator
  { actId                = ActuatorId "Act1" 
  , actName              = Just "My buzzer" 
  , actActuatorKind      = Just $ ActuatorKindId "Buzzer"
  , actActuatorValueType = Just ActBool
  , actValue             = Just $ toJSON True
  } 

instance FromJSON Actuator where
  parseJSON = genericParseJSON $ snakeDrop 3

instance ToJSON Actuator where
  toJSON = genericToJSON $ snakeDrop 3

instance ToSchema Actuator where
   declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.example ?~ toJSON defaultActuator


----------------
-- * Gateways --
----------------

type GatewayName = Text

-- Id of a gateway
newtype GatewayId = GatewayId {unGatewayId :: Text} deriving (Show, Eq, Generic)

--JSON instances
instance ToJSON GatewayId where
  toJSON = genericToJSON (defaultOptions {AT.unwrapUnaryRecords = True})

instance FromJSON GatewayId where
  parseJSON = genericParseJSON (defaultOptions {AT.unwrapUnaryRecords = True})

-- GatewayId is used in Url pieces
instance FromHttpApiData GatewayId where
  parseUrlPiece a = Right $ GatewayId a 

-- GatewayId is used as plain text body
instance MimeRender PlainText GatewayId where
  mimeRender proxy (GatewayId p) = convertString p 

instance MimeUnrender PlainText GatewayId where
  mimeUnrender proxy bs = Right $ GatewayId $ convertString bs 

-- Rendering in Swagger
instance ToSchema GatewayId where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.example ?~ toJSON (GatewayId "MyGatewayId") 

instance ToParamSchema GatewayId

-- | one gateway 
data Gateway = Gateway
  { gwId     :: GatewayId           -- ^ ID of the gateway
  , gwName   :: Maybe GatewayName   -- ^ name of the gateway
  , gwTunnel :: Maybe GatewayTunnel -- ^ gateway tunnel with platform 
  } deriving (Show, Eq, Generic)

defaultGateway = Gateway 
  { gwId      = GatewayId "MyGW"
  , gwName    = Just "My gateway"
  , gwTunnel  = Nothing
  }

--JSON instances
instance FromJSON Gateway where
  parseJSON = genericParseJSON $ snakeDrop 2 

instance ToJSON Gateway where
  toJSON = genericToJSON $ snakeDrop 2

instance ToSchema Gateway where
   declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.example ?~ toJSON defaultGateway 


data GatewayTunnel = GatewayTunnel
  { gwTunnelPort   :: Int
  } deriving (Show, Eq, Generic)

--JSON instances
instance FromJSON GatewayTunnel where
  parseJSON = genericParseJSON $ snakeDrop 2 

instance ToJSON GatewayTunnel where
  toJSON = genericToJSON $ snakeDrop 2

instance ToSchema GatewayTunnel where
   declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.example ?~ toJSON (GatewayTunnel 9999) 

instance MimeUnrender PlainText Int

---------------------
-- * Notifications --
---------------------

-- Id of a gateway
newtype NotifId = NotifId {unNotifId :: Text} deriving (Show, Eq, Generic)

--JSON instances
instance ToJSON NotifId where
  toJSON = genericToJSON (defaultOptions {AT.unwrapUnaryRecords = True})

instance FromJSON NotifId where
  parseJSON = genericParseJSON (defaultOptions {AT.unwrapUnaryRecords = True})

-- is used in Url pieces
instance FromHttpApiData NotifId where
  parseUrlPiece a = Right $ NotifId a 

-- is used as plain text body
instance MimeRender PlainText NotifId where
  mimeRender proxy (NotifId p) = convertString p 

-- Rendering in Swagger
instance ToSchema NotifId
instance ToParamSchema NotifId

-- | one notification
data Notif = Notif
  { notifId          :: Maybe NotifId       -- ^ id of the notification (attributed by the server)
  , notifDescription :: Text                -- ^ Description of the notification
  , notifCondition   :: NotifCondition      -- ^ What is looked at and with which condition 
  , notifAction      :: SocialMessageBatch  -- ^ Where to send the notification
  , notifThrottling  :: Double              -- ^ minimum interval between two messages in seconds
  , notifStatus      :: Maybe O.SubStatus   -- ^ current status of the notification 
  , notifTimesSent   :: Maybe Int
  , notifLastNotif   :: Maybe UTCTime
  , notifLastSuccess :: Maybe UTCTime
  , notifLastFailure :: Maybe UTCTime
  , notifExpires     :: Maybe UTCTime
  } deriving (Show, Eq, Generic)

defaultNotif = Notif
  { notifId          = Nothing 
  , notifDescription = "Test"               
  , notifCondition   = NotifCondition [DeviceId "MyDevice"] [SensorId "TC"] "TC>40"
  , notifAction      = defaultSocialMessageBatch
  , notifThrottling  = 3600
  , notifStatus      = Nothing
  , notifTimesSent   = Nothing
  , notifLastNotif   = Nothing
  , notifLastSuccess = Nothing
  , notifLastFailure = Nothing
  , notifExpires     = parseISO8601 "2016-06-08T18:20:27.873Z"
  }

--JSON instances
instance FromJSON Notif where
  parseJSON = genericParseJSON $ snakeDrop 5

instance ToJSON Notif where
  toJSON = genericToJSON $ snakeDrop 5

--Swagger instances
instance ToSchema Notif where
   declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.example ?~ toJSON defaultNotif 


instance MimeUnrender PlainText O.SubStatus where
  mimeUnrender _ "active"   = Right O.SubActive
  mimeUnrender _ "inactive" = Right O.SubInactive
  mimeUnrender _ _ = Left "cannot decode subscription status. Valid values are \"active\" and \"inactive\""

--Swagger instances
instance ToParamSchema O.SubStatus 

instance ToSchema O.SubStatus where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.example ?~ "inactive" 

-- | notification condition
data NotifCondition = NotifCondition
  { notifDevices    :: [DeviceId]   -- ^ Ids of the devices to watch
  , notifSensors    :: [SensorId]   -- ^ Ids of the sensors to watch 
  , notifExpression :: Text         -- ^ Expression for the condition, such as TC>40
  } deriving (Show, Eq, Generic)

--JSON instances
instance FromJSON NotifCondition where
 parseJSON = genericParseJSON $ snakeDrop 5

instance ToJSON NotifCondition where
  toJSON = genericToJSON $ snakeDrop 5

--Swagger instance
instance ToSchema NotifCondition


---------------
-- * Socials --
---------------

type SocialMessageText = Text

-- Id of a gateway
newtype SocialMessageId = SocialMessageId {unSocialMessageId :: Text} deriving (Show, Eq, Generic)

--JSON instances
instance ToJSON SocialMessageId where
  toJSON = genericToJSON (defaultOptions {AT.unwrapUnaryRecords = True})

instance FromJSON SocialMessageId where
  parseJSON = genericParseJSON (defaultOptions {AT.unwrapUnaryRecords = True})

-- is used in Url pieces
instance FromHttpApiData SocialMessageId where
  parseUrlPiece a = Right $ SocialMessageId a 

-- is used as plain text body
instance MimeRender PlainText SocialMessageId where
  mimeRender proxy (SocialMessageId p) = convertString p 

-- Rendering in Swagger
instance ToSchema SocialMessageId
instance ToParamSchema SocialMessageId

-- channel where the message is sent
data Channel = Twitter | SMS | Voice deriving (Show, Eq, Generic)

instance ToJSON Channel where
  toJSON = genericToJSON $ defaultOptions {AT.constructorTagModifier = map toLower, AT.allNullaryToStringTag = True}

instance FromJSON Channel where
  parseJSON = genericParseJSON $ defaultOptions {AT.constructorTagModifier = map toLower, AT.allNullaryToStringTag = True}

instance ToSchema Channel

-- | One social network message
data SocialMessage = SocialMessage
  { socId        :: Maybe SocialMessageId
  , socUsername  :: Username          -- ^ User name in Keycloak
  , socChannel   :: Channel           -- ^ Channel for the notification 
  , socMessage   :: SocialMessageText -- ^ Text of the message
  , socTimestamp :: Maybe UTCTime
  , socSuccess   :: Maybe Bool
  } deriving (Show, Eq, Generic)

defaultSocialMessage = SocialMessage
  { socId        = Nothing 
  , socUsername  = "cdupont" 
  , socChannel   = Twitter
  , socMessage   = "Test message"
  , socTimestamp = Nothing
  , socSuccess   = Nothing
  }

--JSON instances
instance FromJSON SocialMessage where
  parseJSON = withObject "test" $ \v -> SocialMessage
        <$> v .:? "_id"
        <*> v .: "username" 
        <*> v .: "channel" 
        <*> v .: "message" 
        <*> v .:? "timestamp" 
        <*> v .:? "success" 

instance ToJSON SocialMessage where
  toJSON = genericToJSON $ snakeDrop 3 

--Swagger instances
instance ToSchema SocialMessage where
   declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.example ?~ toJSON defaultSocialMessage 


-- | A message to be sent to several users and socials
data SocialMessageBatch = SocialMessageBatch
  { socBatchUsernames :: [Username]      -- ^ names of the destination users
  , socBatchChannels  :: [Channel]        -- ^ channels where to send the messages
  , socBatchMessage   :: SocialMessageText -- ^ Text of the message 
  } deriving (Show, Eq, Generic)

defaultSocialMessageBatch = SocialMessageBatch 
  { socBatchUsernames = ["cdupont"]
  , socBatchChannels  = [Twitter, SMS] 
  , socBatchMessage   = "Text"
  }


--JSON instances
instance FromJSON SocialMessageBatch where
  parseJSON = genericParseJSON $ snakeDrop 8

instance ToJSON SocialMessageBatch where
  toJSON = genericToJSON $ snakeDrop 8
  
  --Swagger instances
instance ToSchema SocialMessageBatch where
   declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.example ?~ toJSON defaultSocialMessageBatch 


-------------
-- * Users --
-------------

-- Id of a user
newtype UserId = UserId {unUserId :: Text} deriving (Show, Eq, Generic)

--JSON instances
instance ToJSON UserId where
  toJSON = genericToJSON (defaultOptions {AT.unwrapUnaryRecords = True})

instance FromJSON UserId where
  parseJSON = genericParseJSON (defaultOptions {AT.unwrapUnaryRecords = True})

-- is used in Url pieces
instance FromHttpApiData UserId where
  parseUrlPiece a = Right $ UserId a 

--Swagger instance
instance ToSchema UserId
instance ToParamSchema UserId

-- | User 
data User = User
  { userId        :: Maybe UserId   -- ^ The unique user ID 
  , userUsername  :: Username       -- ^ Username
  , userFirstName :: Maybe Text     -- ^ First name
  , userLastName  :: Maybe Text     -- ^ Last name
  , userEmail     :: Maybe Text     -- ^ Email 
  , userPhone     :: Maybe Text     -- ^ Phone with international code: +39... 
  , userFacebook  :: Maybe Text     -- ^ Facebook account
  , userTwitter   :: Maybe Text     -- ^ Twitter account, without the @
  , userSmsCredit :: Maybe Int
  } deriving (Show, Eq, Generic)

defaultUser = User
  { userId        = Nothing 
  , userUsername  = "cdupont"
  , userFirstName = Just "Corentin"
  , userLastName  = Just "Dupont"
  , userEmail     = Nothing
  , userPhone     = Nothing
  , userFacebook  = Nothing
  , userTwitter   = Nothing
  , userSmsCredit = Just 100
  }


instance FromJSON User where
  parseJSON = genericParseJSON $ snakeDrop 4

instance ToJSON User where
  toJSON = genericToJSON $ snakeDrop 4

--Swagger instances
instance ToSchema User where
   declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.example ?~ toJSON defaultUser 


----------------
-- * Projects --
----------------

--Project Id are used in bodies (JSON and PlainText) and URL piece
newtype ProjectId = ProjectId {unProjectId :: Text} deriving (Show, Eq, Generic)

instance ToJSON ProjectId where
  toJSON = genericToJSON (defaultOptions {AT.unwrapUnaryRecords = True})

instance FromJSON ProjectId where
  parseJSON = genericParseJSON (defaultOptions {AT.unwrapUnaryRecords = True})

-- ProjectId is used in URL piece
instance FromHttpApiData ProjectId where
  parseUrlPiece a = Right $ ProjectId a 

-- ProjectId is used as plain text body
instance MimeRender PlainText ProjectId where
  mimeRender proxy (ProjectId p) = convertString p 

instance ToSchema ProjectId where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.example ?~ toJSON (ProjectId "MyProject") 

instance ToParamSchema ProjectId


-- * A project
data Project = Project
  { pId       :: Maybe ProjectId,
    pName     :: Text,
    pDevices  :: [DeviceId],
    pGateways :: [GatewayId] 
  } deriving (Show, Eq, Generic)

defaultProject = Project
  { pId       = Nothing,
    pName     = "MyProject",
    pDevices  = [],
    pGateways = [] 
  }

instance ToJSON Project where
   toJSON (Project pId pName pDev pGate) = 
     object $ (maybe [] (\id -> [("id", toJSON id)]) pId) ++
               ["name"     .= pName,
                "devices"  .= pDev,
                "gateways" .= pGate] 

instance FromJSON Project where
  parseJSON (Object v) = Project <$> v .:? "_id" 
                                 <*> v .:  "name"
                                 <*> v .:  "devices"
                                 <*> v .:  "gateways"
  parseJSON _          = mzero 

instance ToSchema Project where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.example ?~ toJSON defaultProject 


------------------
-- * Ontologies --
------------------

-- * Sensor kinds

--Sensor Kind is a kind of sensor, such as Thermometer, Soil moisture sensor...
newtype SensorKindId = SensorKindId {unSensorKindId :: Text} deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance MimeUnrender PlainText SensorKindId where
  mimeUnrender proxy bs = Right $ SensorKindId $ convertString bs 

instance ToSchema SensorKindId

data SensorKind = SensorKind {
  sdId    :: SensorKindId,
  sdLabel :: Text,
  sdQk    :: [QuantityKindId]
  } deriving (Show, Eq, Generic)

instance FromJSON SensorKind where
  parseJSON (Object v) = SensorKind <$> v .: "id"
                                    <*> v .: "label"
                                    <*> v .: "QK"
  parseJSON _          = mzero 

instance ToJSON SensorKind where
  toJSON = genericToJSON (removeFieldLabelPrefix False "sd")

instance ToSchema SensorKind


-- * Actuator kinds

newtype ActuatorKindId = ActuatorKindId {unActuatorKindId :: Text} deriving (Show, Eq, Generic)

-- JSON instances
instance ToJSON ActuatorKindId where
  toJSON = genericToJSON (defaultOptions {AT.unwrapUnaryRecords = True})

instance FromJSON ActuatorKindId where
  parseJSON = genericParseJSON (defaultOptions {AT.unwrapUnaryRecords = True})

instance MimeUnrender PlainText ActuatorKindId where
  mimeUnrender proxy bs = Right $ ActuatorKindId $ convertString bs 

instance ToSchema ActuatorKindId where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.example ?~ toJSON (ActuatorKindId "Buzzer")

data ActuatorKind = ActuatorKind {
  akId        :: ActuatorKindId,
  akLabel     :: Text,
  akValueType :: [ActuatorValueTypeId]
  } deriving (Show, Eq, Generic)

instance ToJSON ActuatorKind where
  toJSON = genericToJSON (removeFieldLabelPrefix False "ak")

instance FromJSON ActuatorKind where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "ak")

instance ToSchema ActuatorKind


-- Actuator value type denote the kind of data needed to control the actuator
-- TODO: find better name
data ActuatorValueTypeId =  ActString | ActNumber | ActBool | ActNull | ActObject | ActArray deriving (Eq, Generic)

instance Show ActuatorValueTypeId where
  show ActString = "string"
  show ActNumber = "number"
  show ActBool   = "bool"
  show ActNull   = "null"
  show ActObject = "object"
  show ActArray  = "array"

readValueType :: Text -> Maybe ActuatorValueTypeId
readValueType "string" = Just ActString 
readValueType "number" = Just ActNumber 
readValueType "bool"   = Just ActBool 
readValueType "null"   = Just ActNull 
readValueType "object" = Just ActObject 
readValueType "array"  = Just ActArray 
readValueType _        = Nothing

instance ToJSON ActuatorValueTypeId where
  toJSON = genericToJSON $ defaultOptions {AT.constructorTagModifier = unCapitalize . drop 3, AT.allNullaryToStringTag = True}

instance FromJSON ActuatorValueTypeId where
  parseJSON = genericParseJSON $ defaultOptions {AT.constructorTagModifier = unCapitalize . drop 3, AT.allNullaryToStringTag = True}

instance MimeUnrender PlainText ActuatorValueTypeId where
  mimeUnrender _ "string" = Right ActString
  mimeUnrender _ "number" = Right ActNumber
  mimeUnrender _ "bool"   = Right ActBool
  mimeUnrender _ "null"   = Right ActNull
  mimeUnrender _ "object" = Right ActObject
  mimeUnrender _ "array"  = Right ActArray
  mimeUnrender _ _        = Left "Cannot decode value type. Valid values are \"string\", \"number\", \"bool\", \"null\", \"object\" and \"array\"."

instance ToSchema ActuatorValueTypeId where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.example ?~ "string" 

instance ToParamSchema ActuatorValueTypeId

-- * Quantity kinds

newtype QuantityKindId = QuantityKindId {unQuantityKindId :: Text} deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance MimeUnrender PlainText QuantityKindId where
  mimeUnrender proxy bs = Right $ QuantityKindId $ convertString bs 

instance ToSchema QuantityKindId

data QuantityKind = QuantityKind {
  qkId    :: QuantityKindId,
  qkLabel :: Text,
  qkUnits :: [UnitId]
  } deriving (Show, Eq, Generic)

instance FromJSON QuantityKind where
  parseJSON (Object v) = QuantityKind <$> v .: "id"
                                      <*> v .: "label"
                                      <*> v .: "units" 
  parseJSON _          = mzero 

instance ToJSON QuantityKind where
  toJSON = genericToJSON (removeFieldLabelPrefix False "qk")

instance ToSchema QuantityKind


-- * Units

newtype UnitId = UnitId {unUnitId :: Text} deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance MimeUnrender PlainText UnitId where
  mimeUnrender proxy bs = Right $ UnitId $ convertString bs 

instance ToSchema UnitId

data Unit = Unit {
  uId    :: UnitId,
  uLabel :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON Unit where
  parseJSON (Object v) = Unit <$> v .: "id"
                              <*> v .: "label"
  parseJSON _          = mzero 

instance ToJSON Unit where
  toJSON = genericToJSON (removeFieldLabelPrefix False "u")

instance ToSchema Unit


-- All ontologies
data Ontologies = Ontologies {
  sensingDevices   :: [SensorKind],
  actuatingDevices :: [ActuatorKind],
  quantityKinds    :: [QuantityKind],
  units            :: [Unit]
  } deriving (Eq, Show)


-- | Error message 
data Error = Error
  { errorError :: Text -- ^ 
  , errorDescription :: Text -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Error where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "error")

instance ToJSON Error where
  toJSON = genericToJSON (removeFieldLabelPrefix False "error")

instance ToSchema Error

-- * Helpers

unCapitalize :: String -> String
unCapitalize (c:cs) = toLower c : cs
unCapitalize [] = []

snakeDrop :: Int -> Options
snakeDrop n = defaultOptions {fieldLabelModifier = snakeCase . drop n, omitNothingFields = True}

instance ToSchema ResourceId

-- Remove a field label prefix during JSON parsing.
-- Also perform any replacements for special characters.
removeFieldLabelPrefix :: Bool -> String -> Options
removeFieldLabelPrefix forParsing prefix =
  defaultOptions {fieldLabelModifier = fromMaybe (error ("did not find prefix " ++ prefix)) . fmap unCapitalize . stripPrefix prefix . replaceSpecialChars}
  where
    replaceSpecialChars field = foldl (&) field (map mkCharReplacement specialChars)
    specialChars =
      [ ("@", "'At")
      , ("\\", "'Back_Slash")
      , ("<=", "'Less_Than_Or_Equal_To")
      , ("\"", "'Double_Quote")
      , ("[", "'Left_Square_Bracket")
      , ("]", "'Right_Square_Bracket")
      , ("^", "'Caret")
      , ("_", "'Underscore")
      , ("`", "'Backtick")
      , ("!", "'Exclamation")
      , ("#", "'Hash")
      , ("$", "'Dollar")
      , ("%", "'Percent")
      , ("&", "'Ampersand")
      , ("'", "'Quote")
      , ("(", "'Left_Parenthesis")
      , (")", "'Right_Parenthesis")
      , ("*", "'Star")
      , ("+", "'Plus")
      , (",", "'Comma")
      , ("-", "'Dash")
      , (".", "'Period")
      , ("/", "'Slash")
      , (":", "'Colon")
      , ("{", "'Left_Curly_Bracket")
      , ("|", "'Pipe")
      , ("<", "'LessThan")
      , ("!=", "'Not_Equal")
      , ("=", "'Equal")
      , ("}", "'Right_Curly_Bracket")
      , (">", "'GreaterThan")
      , ("~", "'Tilde")
      , ("?", "'Question_Mark")
      , (">=", "'Greater_Than_Or_Equal_To")
      ]
    mkCharReplacement (replaceStr, searchStr) = T.unpack . replacer (T.pack searchStr) (T.pack replaceStr) . T.pack
    replacer =
      if forParsing
        then flip T.replace
        else T.replace

makeLenses ''ServerConfig
makeLenses ''WaziupConfig
makeLenses ''MQTTConfig
makeLenses ''WaziupInfo
makeLenses ''MongoConfig
makeLenses ''PlivoConfig
