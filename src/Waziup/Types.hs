{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

module Waziup.Types where

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Aeson
import Data.Aeson.Types (Options(..), defaultOptions)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Data.Function ((&))


-- | 
data AuthBody = AuthBody
  { authBodyUsername :: Text -- ^ username
  , authBodyPassword :: Text -- ^ password
  } deriving (Show, Eq, Generic)

instance FromJSON AuthBody where
  parseJSON (Object v) = AuthBody <$> v .: "username" <*> v .: "password"
  parseJSON _          = mzero 
  
instance FromJSON Domain where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "domain")
instance ToJSON Domain where
  toJSON = genericToJSON (removeFieldLabelPrefix False "domain")

-- | An entity is a general object with type and name, implementing different userdefined attributes.
data Entity = Entity
  { entityId :: Text -- ^ Unique name (ID) of the entity.
  , entityType :: EntityType -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Entity where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "entity")
instance ToJSON Entity where
  toJSON = genericToJSON (removeFieldLabelPrefix False "entity")

-- | An entity attribute value that could be any type.
data EntityAttr = EntityAttr
  { 
  } deriving (Show, Eq, Generic)

instance FromJSON EntityAttr where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "entityAttr")
instance ToJSON EntityAttr where
  toJSON = genericToJSON (removeFieldLabelPrefix False "entityAttr")

-- | A type of an entity.
newtype EntityType = EntityType Text deriving (Show, Eq, FromJSON, ToJSON, Generic)

-- | An entity is a general object with type and name, implementing different userdefined attributes.
data EntityWithAttr = EntityWithAttr
  { entityWithAttrId :: Text -- ^ Unique name (ID) of the entity.
  , entityWithAttrType :: EntityType -- ^ 
  , entityWithAttrAttribute1 :: [Text] -- ^ An example attribute with string values.
  , entityWithAttrAttribute2 :: [Double] -- ^ An example attribute with number values.
  , entityWithAttrAttributeN :: [EntityWithAttr_attributeN] -- ^ An example attribute with object values.
  } deriving (Show, Eq, Generic)

instance FromJSON EntityWithAttr where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "entityWithAttr")
instance ToJSON EntityWithAttr where
  toJSON = genericToJSON (removeFieldLabelPrefix False "entityWithAttr")

-- | 
data EntityWithAttr_attributeN = EntityWithAttr_attributeN
  { entityWithAttrAttributeNLatitude :: Double -- ^ 
  , entityWithAttrAttributeNLongitude :: Double -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON EntityWithAttr_attributeN where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "entityWithAttrAttributeN")
instance ToJSON EntityWithAttr_attributeN where
  toJSON = genericToJSON (removeFieldLabelPrefix False "entityWithAttrAttributeN")

-- | An entity is a general object with type and name, implementing different userdefined attributes.
data EntityWithCurrentAttr = EntityWithCurrentAttr
  { entityWithCurrentAttrId :: Text -- ^ Unique name (ID) of the entity.
  , entityWithCurrentAttrType :: EntityType -- ^ 
  , entityWithCurrentAttrAttribute1 :: Text -- ^ An example attribute with string values.
  , entityWithCurrentAttrAttribute2 :: Double -- ^ An example attribute with number values.
  , entityWithCurrentAttrAttributeN :: EntityWithCurrentAttr_attributeN -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON EntityWithCurrentAttr where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "entityWithCurrentAttr")
instance ToJSON EntityWithCurrentAttr where
  toJSON = genericToJSON (removeFieldLabelPrefix False "entityWithCurrentAttr")

-- | An example attribute with object values.
data EntityWithCurrentAttr_attributeN = EntityWithCurrentAttr_attributeN
  { entityWithCurrentAttrAttributeNLatitude :: Double -- ^ 
  , entityWithCurrentAttrAttributeNLongitude :: Double -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON EntityWithCurrentAttr_attributeN where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "entityWithCurrentAttrAttributeN")
instance ToJSON EntityWithCurrentAttr_attributeN where
  toJSON = genericToJSON (removeFieldLabelPrefix False "entityWithCurrentAttrAttributeN")

-- | 
data Error = Error
  { errorError :: Text -- ^ 
  , errorDescription :: Text -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Error where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "error")
instance ToJSON Error where
  toJSON = genericToJSON (removeFieldLabelPrefix False "error")

-- | 
data HistoricalValue = HistoricalValue
  { historicalValueId :: Text -- ^ UUID of the sensor
  , historicalValueAttribute'Underscoreid :: Text -- ^ UUID of the measurement
  , historicalValueDatapoint :: MeasurementValue -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON HistoricalValue where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "historicalValue")
instance ToJSON HistoricalValue where
  toJSON = genericToJSON (removeFieldLabelPrefix False "historicalValue")

-- | location is a pair [latitude, longitude] with the coordinates on earth in decimal notation (e.g. [40.418889, 35.89389]).
data Location = Location
  { locationLatitude :: Double -- ^ 
  , locationLongitude :: Double -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Location where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "location")
instance ToJSON Location where
  toJSON = genericToJSON (removeFieldLabelPrefix False "location")

-- | 
data Measurement = Measurement
  { measurementId :: Text -- ^ ID of the measurement
  , measurementName :: Text -- ^ name of the measurement
  , measurementSensing'Underscoredevice :: Text -- ^ sensing platform used for the measurement, from https://github.com/Waziup/waziup-js/blob/master/src/model/SensingDevices.js
  , measurementQuantity'Underscorekind :: Text -- ^ quantity measured, from https://github.com/Waziup/waziup-js/blob/master/src/model/QuantityKinds.js
  , measurementUnit :: Text -- ^ unit of the measurement, from https://github.com/Waziup/waziup-js/blob/master/src/model/Units.js
  , measurementLast'Underscorevalue :: MeasurementValue -- ^ last value received by the platform
  } deriving (Show, Eq, Generic)

instance FromJSON Measurement where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "measurement")
instance ToJSON Measurement where
  toJSON = genericToJSON (removeFieldLabelPrefix False "measurement")

-- | 
data MeasurementValue = MeasurementValue
  { measurementValueValue :: Float -- ^ value of the measurement
  , measurementValueTimestamp :: Text -- ^ time of the measurement
  , measurementValueDate'Underscorereceived :: Text -- ^ time at which the measurement has been received on the Cloud
  } deriving (Show, Eq, Generic)

instance FromJSON MeasurementValue where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "measurementValue")
instance ToJSON MeasurementValue where
  toJSON = genericToJSON (removeFieldLabelPrefix False "measurementValue")

-- | 
data Notification = Notification
  { notificationId :: Text -- ^ id of the notification (attributed by the server)
  , notificationDescription :: Text -- ^ Description of the notification
  , notificationSubject :: NotificationSubject -- ^ 
  , notificationNotification :: SocialMessageBatch -- ^ 
  , notificationThrottling :: Double -- ^ minimum interval between two messages in seconds
  } deriving (Show, Eq, Generic)

instance FromJSON Notification where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "notification")
instance ToJSON Notification where
  toJSON = genericToJSON (removeFieldLabelPrefix False "notification")

-- | 
data NotificationCondition = NotificationCondition
  { notificationConditionAttrs :: [Text] -- ^ 
  , notificationConditionExpression :: Text -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON NotificationCondition where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "notificationCondition")
instance ToJSON NotificationCondition where
  toJSON = genericToJSON (removeFieldLabelPrefix False "notificationCondition")

-- | 
data NotificationSubject = NotificationSubject
  { notificationSubjectEntityNames :: [Text] -- ^ 
  , notificationSubjectCondition :: NotificationCondition -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON NotificationSubject where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "notificationSubject")
instance ToJSON NotificationSubject where
  toJSON = genericToJSON (removeFieldLabelPrefix False "notificationSubject")

-- | 
data Perm = Perm
  { permissionResource :: Text -- ^ 
  , permissionScopes :: [Text] -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON Perm where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "permission")
instance ToJSON Perm where
  toJSON = genericToJSON (removeFieldLabelPrefix False "permission")

-- | 
data Sensor = Sensor
  { sensorId :: Text -- ^ Unique ID of the sensor node
  , sensorGateway'Underscoreid :: Text -- ^ Unique ID of the gateway
  , sensorName :: Text -- ^ name of the sensor node
  , sensorOwner :: Text -- ^ owner of the sensor node
  , sensorMeasurements :: [Measurement] -- ^ 
  , sensorLocation :: Location -- ^ 
  , sensorDomain :: Text -- ^ the domain of this sensor.
  , sensorDate'Underscorecreated :: Text -- ^ creation date of the sensor node
  , sensorDate'Underscoreupdated :: Text -- ^ last update date of the sensor node
  } deriving (Show, Eq, Generic)

instance FromJSON Sensor where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "sensor")
instance ToJSON Sensor where
  toJSON = genericToJSON (removeFieldLabelPrefix False "sensor")

-- | One social network message
data SocialMessage = SocialMessage
  { socialMessageUsername :: Text -- ^ User name in Keycloak
  , socialMessageChannel :: Text -- ^ 
  , socialMessageMessage :: Text -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON SocialMessage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "socialMessage")
instance ToJSON SocialMessage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "socialMessage")

-- | A message to be sent to several users and socials
data SocialMessageBatch = SocialMessageBatch
  { socialMessageBatchUsernames :: [Text] -- ^ names of the destination users
  , socialMessageBatchChannels :: [Text] -- ^ channels where to send the messages
  , socialMessageBatchMessage :: Text -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON SocialMessageBatch where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "socialMessageBatch")
instance ToJSON SocialMessageBatch where
  toJSON = genericToJSON (removeFieldLabelPrefix False "socialMessageBatch")

-- | 
data User = User
  { userId :: Text -- ^ 
  , userUsername :: Text -- ^ 
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

-- Remove a field label prefix during JSON parsing.
-- Also perform any replacements for special characters.
removeFieldLabelPrefix :: Bool -> String -> Options
removeFieldLabelPrefix forParsing prefix =
  defaultOptions
  {fieldLabelModifier = fromMaybe (error ("did not find prefix " ++ prefix)) . stripPrefix prefix . replaceSpecialChars}
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
