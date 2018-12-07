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
import           Data.String.Conversions
import           Control.Lens hiding ((.=))
import           Control.Monad
import           Control.Monad.Except (ExceptT, throwError)
import           Control.Monad.Catch as C
import           Control.Monad.Reader
import           Servant
import           Servant.Swagger
import           Servant.API.Flatten
import           Keycloak as KC hiding (info, warn, debug, Scope) 
import           GHC.Generics (Generic)
import qualified Database.MongoDB as DB
import qualified Orion.Types as O
import qualified Mongo.Types as M


-- Waziup Monad
type Waziup = ReaderT WaziupInfo Servant.Handler

data WaziupInfo = WaziupInfo {
  _dbPipe :: DB.Pipe,
  _waziupConfig :: WaziupConfig,
  _ontologies   :: Ontologies
  }

-- * Config
data WaziupConfig = WaziupConfig {
  _serverConf   :: ServerConfig,
  _mongoConf    :: M.MongoConfig,
  _keycloakConf :: KCConfig,
  _orionConf    :: O.OrionConfig
  } deriving (Eq, Show)

-- | Server or client configuration, specifying the host and port to query or serve on.
data ServerConfig = ServerConfig
  { _serverHost :: String   -- ^ Hostname to serve on, e.g. "127.0.0.1"
  , _serverPort :: Int      -- ^ Port to serve on, e.g. 8080
  } deriving (Eq, Show)

defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig {
  _serverHost = "http://localhost:3000",
  _serverPort = 3000
  }

data Ontologies = Ontologies {
  sensingDevices :: [SensorKind],
  quantityKinds  :: [QuantityKind],
  units          :: [Unit]
  } deriving (Eq, Show)

-- * Authentication & authorization

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

data Scope = DevicesCreate
           | DevicesUpdate
           | DevicesView
           | DevicesDelete
           | DevicesDataCreate
           | DevicesDataView
   deriving (Generic, Eq)

instance ToJSON Scope where
  toJSON = toJSON . show
instance FromJSON Scope
instance ToSchema Scope

readScope :: Text -> Maybe Scope
readScope "devices:create"      = Just DevicesCreate    
readScope "devices:update"      = Just DevicesUpdate    
readScope "devices:view"        = Just DevicesView      
readScope "devices:delete"      = Just DevicesDelete    
readScope "devices-data:create" = Just DevicesDataCreate
readScope "devices-data:view"   = Just DevicesDataView  
readScope _                     = Nothing

instance Show Scope where
  show DevicesCreate     = "devices:create"       
  show DevicesUpdate     = "devices:update"       
  show DevicesView       = "devices:view"         
  show DevicesDelete     = "devices:delete"       
  show DevicesDataCreate = "devices-data:create"  
  show DevicesDataView   = "devices-data:view"    

-- * Devices

-- Id of a device
newtype DeviceId = DeviceId {unDeviceId :: Text} deriving (Show, Eq, Generic)

instance ToSchema DeviceId where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.example ?~ toJSON ("MyDevice" :: Text)

instance ToParamSchema DeviceId

instance ToJSON DeviceId where
  toJSON = genericToJSON (defaultOptions {AT.unwrapUnaryRecords = True})

instance FromJSON DeviceId where
  parseJSON = genericParseJSON (defaultOptions {AT.unwrapUnaryRecords = True})

instance FromHttpApiData DeviceId where
  parseUrlPiece a = Right $ DeviceId a 

instance ToHttpApiData DeviceId where
  toUrlPiece (DeviceId a) = a

-- Id of a gateway
newtype GatewayId = GatewayId {unGatewayId :: Text} deriving (Show, Eq, Generic)

instance ToSchema GatewayId where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.example ?~ toJSON ("MyGatewayId" :: Text) 

instance ToParamSchema GatewayId

instance MimeUnrender PlainText GatewayId where
  mimeUnrender proxy bs = Right $ GatewayId $ convertString bs 

instance ToJSON GatewayId where
  toJSON = genericToJSON (defaultOptions {AT.unwrapUnaryRecords = True})

instance FromJSON GatewayId where
  parseJSON = genericParseJSON (defaultOptions {AT.unwrapUnaryRecords = True})

instance FromHttpApiData GatewayId where
  parseUrlPiece a = Right $ GatewayId a 

instance ToHttpApiData GatewayId where
  toUrlPiece (GatewayId a) = a

type DeviceName    = Text
type Domain        = Text
type DevicesQuery  = Text
type DevicesLimit  = Int
type DevicesOffset = Int

instance ToSchema ResourceId

-- | one device 
data Device = Device
  { devId           :: DeviceId   -- ^ Unique ID of the device node
  , devGatewayId    :: Maybe GatewayId  -- ^ Unique ID of the gateway
  , devName         :: Maybe DeviceName -- ^ name of the device node
  , devLocation     :: Maybe Location
  , devDomain       :: Maybe Domain     -- ^ the domain of this device.
  , devVisibility   :: Maybe Visibility
  , devMeasurements :: [Measurement]
  , devOwner        :: Maybe Username   -- ^ owner of the device node (output only)
  , devDateCreated  :: Maybe UTCTime    -- ^ creation date of the device (output only)
  , devDateModified :: Maybe UTCTime    -- ^ last update date of the device node (output only)
  , devKeycloakId   :: Maybe ResourceId -- ^ The is of the resource in Keycloak
  } deriving (Show, Eq, Generic)

defaultDevice = Device
  { devId           = DeviceId "MyDevice"
  , devGatewayId    = Just $ GatewayId "ea0541de1ab7132a1d45b85f9b2139f5" 
  , devName         = Just "My weather station" 
  , devLocation     = Just defaultLocation 
  , devDomain       = Just "waziup" 
  , devVisibility   = Just Public
  , devMeasurements = [defaultMeasurement]
  , devOwner        = Nothing
  , devDateCreated  = Nothing
  , devDateModified = Nothing
  , devKeycloakId   = Nothing 
  }

instance ToJSON Device where
  toJSON = genericToJSON (aesonDrop 3 snakeCase) {omitNothingFields = True}

instance FromJSON Device where
  parseJSON = genericParseJSON $ aesonDrop 3 snakeCase

instance ToSchema Device where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.example ?~ toJSON defaultDevice 


data Visibility = Public | Private
  deriving (Eq, Generic)

instance ToJSON Visibility where
  toJSON Public  = "public" 
  toJSON Private = "private" 
instance FromJSON Visibility where
  parseJSON = Aeson.withText "String" (\x -> return $ fromJust $ readVisibility x)
instance ToParamSchema Visibility
instance ToSchema Visibility
instance MimeRender PlainText Visibility
instance MimeUnrender PlainText Visibility

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

defaultLocation :: Location
defaultLocation = Location (Latitude 5.36) (Longitude 4.0083)

instance FromJSON Location where
  parseJSON = genericParseJSON defaultOptions
instance ToJSON Location where
  toJSON = genericToJSON defaultOptions
instance ToSchema Location where
   declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.example ?~ toJSON defaultLocation 


-- * Measurements

-- Id of a measurement
newtype MeasId = MeasId {unMeasId :: Text} deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance ToSchema MeasId where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.example ?~ toJSON ("TC" :: Text) 

instance ToParamSchema MeasId

instance MimeRender PlainText MeasId

instance MimeUnrender PlainText MeasId

instance FromHttpApiData MeasId where
  parseUrlPiece a = Right $ MeasId a 

instance ToHttpApiData MeasId where
  toUrlPiece (MeasId a) = a


type MeasName      = Text

-- | one measurement 
data Measurement = Measurement
  { measId            :: MeasId                 -- ^ ID of the measurement
  , measName          :: Maybe MeasName         -- ^ name of the measurement
  , measSensorKind    :: Maybe SensorKindId     -- ^ sensing platform used for the measurement, from https://github.com/Waziup/waziup-js/blob/master/src/model/SensingDevices.js
  , measQuantityKind  :: Maybe QuantityKindId   -- ^ quantity measured, from https://github.com/Waziup/waziup-js/blob/master/src/model/QuantityKinds.js
  , measUnit          :: Maybe UnitId           -- ^ unit of the measurement, from https://github.com/Waziup/waziup-js/blob/master/src/model/Units.js
  , measLastValue     :: Maybe MeasurementValue -- ^ last value received by the platform
  } deriving (Show, Eq, Generic)

defaultMeasurement = Measurement 
  { measId            = MeasId "TC1" 
  , measName          = Just "My garden temperature" 
  , measSensorKind    = Just $ SensorKindId "Thermometer" 
  , measQuantityKind  = Just $ QuantityKindId "AirTemperature" 
  , measUnit          = Just $ UnitId "DegreeCelsius"
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
  , measDateReceived = Nothing
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

-- * Data points

-- | one datapoint 
data Datapoint = Datapoint
  { dataDeviceId :: DeviceId               -- ^ ID of the device
  , dataMeasId   :: MeasId                 -- ^ ID of the measurement
  , dataValue    :: MeasurementValue -- ^ last value received by the platform
  } deriving (Show, Eq, Generic)

defaultDatapoint = Datapoint 
  { dataDeviceId = DeviceId "MyDevice90"   -- ^ ID of the device
  , dataMeasId   = MeasId "TC1"            -- ^ ID of the measurement
  , dataValue    = defaultMeasurementValue -- ^ last value received by the platform
  }

instance FromJSON Datapoint where
  parseJSON = genericParseJSON $ aesonDrop 4 snakeCase

instance ToJSON Datapoint where
  toJSON = genericToJSON (aesonDrop 4 snakeCase) {omitNothingFields = True}

instance ToSchema Datapoint where
   declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
        & mapped.schema.example ?~ toJSON defaultDatapoint 

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
  { notificationSubjectEntityNames :: [DeviceId]          -- ^ Ids of the devices to watch
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


-- * Projects

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


-- * Ontologies

newtype SensorKindId = SensorKindId {unSensorKindId :: Text} deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance ToSchema SensorKindId
instance MimeRender PlainText SensorKindId
instance MimeUnrender PlainText SensorKindId where
  mimeUnrender proxy bs = Right $ SensorKindId $ convertString bs 


newtype QuantityKindId = QuantityKindId {unQuantityKindId :: Text} deriving (Show, Eq, Generic, ToJSON, FromJSON)
instance ToSchema QuantityKindId
instance MimeRender PlainText QuantityKindId
instance MimeUnrender PlainText QuantityKindId where
  mimeUnrender proxy bs = Right $ QuantityKindId $ convertString bs 

newtype UnitId = UnitId {unUnitId :: Text} deriving (Show, Eq, Generic, ToJSON, FromJSON)
instance ToSchema UnitId
instance MimeRender PlainText UnitId
instance MimeUnrender PlainText UnitId where
  mimeUnrender proxy bs = Right $ UnitId $ convertString bs 

data SensorKind = SensorKind {
  sdId    :: SensorKindId,
  sdLabel :: Text,
  sdQk    :: [QuantityKindId]
  } deriving (Show, Eq, Generic)

parseSDI :: Parse e SensorKind
parseSDI = do
    id    <- AB.key "id" asText
    label <- AB.key "label" asText
    qks   <- AB.key "QK" (eachInArray asText) 
    return $ SensorKind (SensorKindId id) label (map QuantityKindId qks)

instance ToJSON SensorKind where
  toJSON = genericToJSON (removeFieldLabelPrefix False "sd")

instance ToSchema SensorKind

data QuantityKind = QuantityKind {
  qkId    :: QuantityKindId,
  qkLabel :: Text,
  qkUnits :: [UnitId]
  } deriving (Show, Eq, Generic)

parseQKI :: Parse e QuantityKind
parseQKI = do
    id    <- AB.key "id" asText
    label <- AB.key "label" asText
    us    <- AB.key "units" (eachInArray asText) 
    return $ QuantityKind (QuantityKindId id) label (map UnitId us)

instance ToJSON QuantityKind where
  toJSON = genericToJSON (removeFieldLabelPrefix False "qk")

instance ToSchema QuantityKind

data Unit = Unit {
  uId    :: UnitId,
  uLabel :: Text
  } deriving (Show, Eq, Generic)

parseUnit :: Parse e Unit
parseUnit = do
    id    <- AB.key "id" asText
    label <- AB.key "label" asText
    return $ Unit (UnitId id) label

instance ToJSON Unit where
  toJSON = genericToJSON (removeFieldLabelPrefix False "u")

instance ToSchema Unit

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

makeLenses ''ServerConfig
makeLenses ''WaziupConfig
makeLenses ''WaziupInfo
