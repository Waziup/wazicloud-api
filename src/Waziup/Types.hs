{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Waziup.Types where

import           Data.List (stripPrefix)
import           Data.Maybe (fromMaybe)
import           Data.Aeson as Aeson
import           Data.Aeson.Types as AT (Options(..), defaultOptions, Pair)
import           Data.Aeson.Casing
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
import           Data.Aeson.BetterErrors as AB
import           Data.Swagger
import           Data.Swagger.Internal
import           Data.Swagger.Lens
import           Control.Lens hiding ((.=))
import           Control.Monad
import           Control.Monad.Except (ExceptT, throwError)
import           Control.Monad.Catch as C
import           Control.Monad.Reader
import           Servant
import           Servant.Swagger
import           Servant.API.Flatten
import           Keycloak as KC hiding (info, warn, debug, Scope, Username) 
import           GHC.Generics (Generic)
import qualified Database.MongoDB as DB
import qualified Orion.Types as O
import qualified Mongo.Types as M


-- Waziup Monad
type Waziup = ReaderT WaziupInfo Servant.Handler

data WaziupInfo = WaziupInfo {
  dbPipe :: DB.Pipe,
  waziupConfig :: WaziupConfig,
  ontologies   :: Ontologies
  }

-- * Config
data WaziupConfig = WaziupConfig {
  serverConf   :: ServerConfig,
  mongoConf    :: M.MongoConfig,
  keycloakConf :: KCConfig,
  orionConf    :: O.OrionConfig
  } deriving (Eq, Show)

-- | Server or client configuration, specifying the host and port to query or serve on.
data ServerConfig = ServerConfig
  { configHost :: String   -- ^ Hostname to serve on, e.g. "127.0.0.1"
  , configPort :: Int      -- ^ Port to serve on, e.g. 8080
  } deriving (Eq, Show)

defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig {
  configHost = "http://localhost:3000",
  configPort = 3000
  }

data Ontologies = Ontologies {
  sensingDevices :: [SensingDeviceInfo],
  quantityKinds  :: [QuantityKindInfo],
  units          :: [UnitInfo]
  } deriving (Eq, Show)

-- * Authentication & authorization

type Username = Text
type Password = Text

-- | Auth details 
data AuthBody = AuthBody
  { authBodyUsername :: Username
  , authBodyPassword :: Password
  } deriving (Show, Eq, Generic)

instance ToJSON AuthBody where
  toJSON = genericToJSON (removeFieldLabelPrefix False "authBody")
instance FromJSON AuthBody where
  parseJSON (Object v) = AuthBody <$> v .: "username" <*> v .: "password"
  parseJSON _          = mzero 

instance ToSchema AuthBody where
   declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.example ?~ toJSON (AuthBody "cdupont" "password")

-- | Permission
data Perm = Perm
  { permResource :: Text -- ^ 
  , permScopes :: [Scope] -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Perm where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "perm")
instance ToJSON Perm where
  toJSON = genericToJSON (removeFieldLabelPrefix False "perm")
instance ToSchema Perm

data Scope = SensorsCreate
           | SensorsUpdate
           | SensorsView
           | SensorsDelete
           | SensorsDataCreate
           | SensorsDataView
   deriving (Generic, Eq)

instance ToJSON Scope where
  toJSON = toJSON . show
instance FromJSON Scope
instance ToSchema Scope

readScope :: Text -> Maybe Scope
readScope "sensors:create"      = Just SensorsCreate    
readScope "sensors:update"      = Just SensorsUpdate    
readScope "sensors:view"        = Just SensorsView      
readScope "sensors:delete"      = Just SensorsDelete    
readScope "sensors-data:create" = Just SensorsDataCreate
readScope "sensors-data:view"   = Just SensorsDataView  
readScope _                     = Nothing

instance Show Scope where
  show SensorsCreate     = "sensors:create"       
  show SensorsUpdate     = "sensors:update"       
  show SensorsView       = "sensors:view"         
  show SensorsDelete     = "sensors:delete"       
  show SensorsDataCreate = "sensors-data:create"  
  show SensorsDataView   = "sensors-data:view"    

-- * Sensors

type SensorId      = Text
type SensorName    = Text
type GatewayId     = Text
type Domain        = Text
type SensorsQuery  = Text
type SensorsLimit  = Int
type SensorsOffset = Int

-- | one sensor 
data Sensor = Sensor
  { senId           :: SensorId   -- ^ Unique ID of the sensor node
  , senGatewayId    :: Maybe GatewayId  -- ^ Unique ID of the gateway
  , senName         :: Maybe SensorName -- ^ name of the sensor node
  , senLocation     :: Maybe Location
  , senDomain       :: Maybe Domain     -- ^ the domain of this sensor.
  , senVisibility   :: Maybe Visibility
  , senMeasurements :: [Measurement]
  , senOwner        :: Maybe Text       -- ^ owner of the sensor node (output only)
  , senDateCreated  :: Maybe UTCTime    -- ^ creation date of the sensor node (output only)
  , senDateModified :: Maybe UTCTime    -- ^ last update date of the sensor nodei (output only)
  , senKeycloakId   :: Maybe ResourceId -- ^ The is of the resource in Keycloak
  } deriving (Show, Eq, Generic)

defaultSensor = Sensor
  { senId           = "MyDevice"
  , senGatewayId    = Just "ea0541de1ab7132a1d45b85f9b2139f5" 
  , senName         = Just "My weather station" 
  , senLocation     = Just defaultLocation 
  , senDomain       = Just "waziup" 
  , senVisibility   = Just Public
  , senMeasurements = [defaultMeasurement]
  , senOwner        = Nothing
  , senDateCreated  = Nothing
  , senDateModified = Nothing
  , senKeycloakId   = Nothing 
  }

instance ToJSON Sensor where
   toJSON = genericToJSON (aesonDrop 3 snakeCase) {omitNothingFields = True}
instance FromJSON Sensor where
   parseJSON = genericParseJSON $ aesonDrop 3 snakeCase
instance ToSchema Sensor where
   declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.example ?~ toJSON defaultSensor 
instance ToSchema ResourceId

data Visibility = Public | Private
  deriving (Eq, Generic)

instance ToJSON Visibility where
  toJSON Public  = "public" 
  toJSON Private = "private" 
instance FromJSON Visibility where
  parseJSON = Aeson.withText "String" (\x -> return $ fromJust $ readVisibility x)
instance ToSchema Visibility

instance Show Visibility where
  show Public = "public"
  show Private = "private"

readVisibility :: Text -> Maybe Visibility
readVisibility "public" = Just Public
readVisibility "private" = Just Private
readVisibility _ = Nothing

-- * Location

newtype Latitude  = Latitude  Double deriving (Show, Eq, Generic, ToJSON, FromJSON)
newtype Longitude = Longitude Double deriving (Show, Eq, Generic, ToJSON, FromJSON)
instance ToSchema Longitude
instance ToSchema Latitude

-- | location is a pair [latitude, longitude] with the coordinates on earth in decimal notation (e.g. [40.418889, 35.89389]).
data Location = Location
  { latitude  :: Latitude
  , longitude :: Longitude
  } deriving (Show, Eq, Generic)

defaultLocation = Location (Latitude 5.36) (Longitude 4.0083)

instance FromJSON Location where
  parseJSON = genericParseJSON defaultOptions
instance ToJSON Location where
  toJSON = genericToJSON defaultOptions
instance ToSchema Location where
   declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.example ?~ toJSON defaultLocation 


-- * Measurements

type MeasId        = Text
type MeasName      = Text
type SensingDevice = Text
type QuantityKind  = Text
type Unit          = Text

-- | one measurement 
data Measurement = Measurement
  { measId            :: MeasId                 -- ^ ID of the measurement
  , measName          :: Maybe MeasName         -- ^ name of the measurement
  , measSensingDevice :: Maybe SensingDevice    -- ^ sensing platform used for the measurement, from https://github.com/Waziup/waziup-js/blob/master/src/model/SensingDevices.js
  , measQuantityKind  :: Maybe QuantityKind     -- ^ quantity measured, from https://github.com/Waziup/waziup-js/blob/master/src/model/QuantityKinds.js
  , measUnit          :: Maybe Unit             -- ^ unit of the measurement, from https://github.com/Waziup/waziup-js/blob/master/src/model/Units.js
  , measLastValue     :: Maybe MeasurementValue -- ^ last value received by the platform
  } deriving (Show, Eq, Generic)

defaultMeasurement = Measurement 
  { measId            = "TC1" 
  , measName          = Just "My garden temperature" 
  , measSensingDevice = Just "Thermometer" 
  , measQuantityKind  = Just "AirTemperature" 
  , measUnit          = Just "DegreeCelsius"
  , measLastValue     = Nothing
  } 

instance FromJSON Measurement where
  parseJSON = genericParseJSON $ aesonDrop 4 snakeCase 
instance ToJSON Measurement where
  toJSON = genericToJSON (aesonDrop 4 snakeCase) {omitNothingFields = True}
instance ToSchema Measurement where
   declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.example ?~ toJSON defaultMeasurement 


-- | measurement value 
data MeasurementValue = MeasurementValue
  { measValue        :: Value          -- ^ value of the measurement
  , measTimestamp    :: Maybe UTCTime  -- ^ time of the measurement
  , measDateReceived :: Maybe UTCTime  -- ^ time at which the measurement has been received on the Cloud
  } deriving (Show, Eq, Generic)

defaultMeasurementValue = MeasurementValue 
  { measValue        = Number 25
  , measTimestamp    = parseISO8601 "2016-06-08T18:20:27.873Z"
  , measDateReceived = parseISO8601 "2016-06-08T18:20:27.873Z"
  }

instance FromJSON MeasurementValue where
  parseJSON = genericParseJSON $ aesonDrop 4 snakeCase
instance ToJSON MeasurementValue where
  toJSON = genericToJSON (aesonDrop 4 snakeCase) {omitNothingFields = True}
instance ToSchema MeasurementValue where
   declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.example ?~ toJSON defaultMeasurementValue 

instance ToSchema Value where
  declareNamedSchema _ = pure (NamedSchema (Just "Value") (mempty & type_ .~ SwaggerObject))
  


-- * Notifications

type NotifId  = Text

-- | one notification
data Notification = Notification
  { notifId           :: NotifId -- ^ id of the notification (attributed by the server)
  , notifDescription  :: Text    -- ^ Description of the notification
  , notifSubject      :: NotificationSubject -- ^ 
  , notifNotification :: SocialMessageBatch -- ^ 
  , notifThrottling   :: Double -- ^ minimum interval between two messages in seconds
  } deriving (Show, Eq, Generic)

instance FromJSON Notification where
  parseJSON = genericParseJSON $ aesonDrop 5 snakeCase
instance ToJSON Notification where
  toJSON = genericToJSON $ aesonDrop 5 snakeCase
instance ToSchema Notification

-- | notification condition
data NotificationCondition = NotificationCondition
  { notifCondAttrs      :: [MeasId] -- ^ Ids of the measurements to watch 
  , notifCondExpression :: Text     -- ^ Expression for the condition, such as TC>40
  } deriving (Show, Eq, Generic)

instance FromJSON NotificationCondition where
  parseJSON = genericParseJSON $ aesonDrop 5 snakeCase
instance ToJSON NotificationCondition where
  toJSON = genericToJSON (removeFieldLabelPrefix False "notificationCondition")
instance ToSchema NotificationCondition

-- | notification subject
data NotificationSubject = NotificationSubject
  { notificationSubjectEntityNames :: [SensorId]          -- ^ Ids of the sensors to watch
  , notificationSubjectCondition :: NotificationCondition -- ^ Condition of the notification
  } deriving (Show, Eq, Generic)

instance FromJSON NotificationSubject where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "notificationSubject")
instance ToJSON NotificationSubject where
  toJSON = genericToJSON (removeFieldLabelPrefix False "notificationSubject")
instance ToSchema NotificationSubject


-- * Socials

data Channel = Twitter | SMS | Voice deriving (Show, Eq, Generic)
type SocialMessageText = Text

instance ToJSON Channel
instance FromJSON Channel
instance ToSchema Channel

-- | One social network message
data SocialMessage = SocialMessage
  { socialMessageUsername :: Username          -- ^ User name in Keycloak
  , socialMessageChannel  :: Channel           -- ^ Channel for the notification 
  , socialMessageText     :: SocialMessageText -- ^ Text of the message
  } deriving (Show, Eq, Generic)

instance FromJSON SocialMessage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "socialMessage")
instance ToJSON SocialMessage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "socialMessage")
instance ToSchema SocialMessage

-- | A message to be sent to several users and socials
data SocialMessageBatch = SocialMessageBatch
  { socialMessageBatchUsernames :: [Username]      -- ^ names of the destination users
  , socialMessageBatchChannels :: [Channel]        -- ^ channels where to send the messages
  , socialMessageBatchMessage :: SocialMessageText -- ^ Text of the message 
  } deriving (Show, Eq, Generic)

instance FromJSON SocialMessageBatch where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "socialMessageBatch")
instance ToJSON SocialMessageBatch where
  toJSON = genericToJSON (removeFieldLabelPrefix False "socialMessageBatch")
instance ToSchema SocialMessageBatch

-- | User 
data User = User
  { userId :: Text -- ^ 
  , userUsername :: Username -- ^ 
  , userFirstName :: Text -- ^ 
  , userLastName :: Text -- ^ 
  , userSubservice :: Text -- ^ 
  , userEmail :: Text -- ^ 
  , userPhone :: Text -- ^ 
  , userAddress :: Text -- ^ 
  , userFacebook :: Text -- ^ 
  , userTwitter :: Text -- ^ 
  , userRoles :: Text -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON User where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "user")
instance ToJSON User where
  toJSON = genericToJSON (removeFieldLabelPrefix False "user")
instance ToSchema User

data HistoricalValue = HistoricalValue
  { historicalValueId            :: Text -- ^ UUID of the sensor
  , historicalValueAttribute'Underscoreid :: Text -- ^ UUID of the measurement
  , historicalValueDatapoint :: MeasurementValue -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON HistoricalValue where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "historicalValue")
instance ToJSON HistoricalValue where
  toJSON = genericToJSON (removeFieldLabelPrefix False "historicalValue")
instance ToSchema HistoricalValue

-- * Projects
type DeviceId = Text
type ProjectId = Text

-- * A project
data Project = Project
  { pId       :: Maybe ProjectId,
    pName     :: Text,
    pDevices  :: [DeviceId],
    pGateways :: [GatewayId] 
  } deriving (Show, Eq, Generic)

instance ToJSON Project where
   toJSON (Project pId pName pDev pGate) = 
     object $ ["id"       .= pId,
               "name"     .= pName,
               "devices"  .= pDev,
               "gateways" .= pGate]
instance FromJSON Project where
  parseJSON (Object v) = Project <$> v .:? "_id" 
                                 <*> v .:  "name"
                                 <*> v .:  "devices"
                                 <*> v .:  "gateways"
  parseJSON _          = mzero 

instance ToSchema Project


-- * Ontologies

data SensingDeviceInfo = SensingDeviceInfo {
  sdId :: SensingDevice,
  sdLabel :: Text,
  sdQK :: [QuantityKind]
  } deriving (Show, Eq, Generic)

parseSDI :: Parse e SensingDeviceInfo
parseSDI = do
    id   <- AB.key "id" asText
    label <- AB.key "label" asText
    qks   <- AB.key "QK" (eachInArray asText) 
    return $ SensingDeviceInfo id label qks

instance ToJSON SensingDeviceInfo where
  toJSON = genericToJSON (removeFieldLabelPrefix False "sd")

instance ToSchema SensingDeviceInfo

data QuantityKindInfo = QuantityKindInfo {
  qkId :: QuantityKind,
  qkLabel :: Text,
  qkUnits :: [Unit]
  } deriving (Show, Eq, Generic)

parseQKI :: Parse e QuantityKindInfo
parseQKI = do
    id   <- AB.key "id" asText
    label <- AB.key "label" asText
    us   <- AB.key "units" (eachInArray asText) 
    return $ QuantityKindInfo id label us

instance ToJSON QuantityKindInfo where
  toJSON = genericToJSON (removeFieldLabelPrefix False "qk")

instance ToSchema QuantityKindInfo

data UnitInfo = UnitInfo {
  uId :: Unit,
  uLabel :: Text
  } deriving (Show, Eq, Generic)

parseUnit :: Parse e UnitInfo
parseUnit = do
    id   <- AB.key "id" asText
    label <- AB.key "label" asText
    return $ UnitInfo id label

instance ToJSON UnitInfo where
  toJSON = genericToJSON (removeFieldLabelPrefix False "u")

instance ToSchema UnitInfo

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


-- Remove a field label prefix during JSON parsing.
-- Also perform any replacements for special characters.
removeFieldLabelPrefix :: Bool -> String -> Options
removeFieldLabelPrefix forParsing prefix =
  defaultOptions
  {AT.fieldLabelModifier = fromMaybe (error ("did not find prefix " ++ prefix)) . fmap unCapitalize . stripPrefix prefix . replaceSpecialChars}
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
