{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Types where

import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Aeson
import Data.Aeson.Types (Options(..), defaultOptions)
import Data.Aeson.Casing
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Data.Function ((&))
import Data.Time
import Control.Monad
import Data.Time.ISO8601

-- | 
data AuthBody = AuthBody
  { authBodyUsername :: Text -- ^ username
  , authBodyPassword :: Text -- ^ password
  } deriving (Show, Eq, Generic)

instance FromJSON AuthBody where
  parseJSON (Object v) = AuthBody <$> v .: "username" <*> v .: "password"
  parseJSON _          = mzero 
  
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

-- | 
data Sensor = Sensor
  { sensorId :: Text                    -- ^ Unique ID of the sensor node
  , sensorGatewayId :: Maybe Text       -- ^ Unique ID of the gateway
  , sensorName :: Maybe Text            -- ^ name of the sensor node
  , sensorOwner :: Maybe Text           -- ^ owner of the sensor node
  , sensorMeasurements :: [Measurement]
  , sensorLocation :: Maybe Location
  , sensorDomain :: Maybe Text          -- ^ the domain of this sensor.
  , sensorDateCreated :: Maybe UTCTime     -- ^ creation date of the sensor node
  , sensorDateUpdated :: Maybe UTCTime     -- ^ last update date of the sensor nodei
  , sensorVisibility :: Maybe Visibility
  } deriving (Show, Eq, Generic)

instance ToJSON Sensor where
   toJSON = genericToJSON $ aesonPrefix snakeCase
instance FromJSON Sensor where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase


data Visibility = Public | Private
  deriving (Eq, Generic)

instance ToJSON Visibility where
   toJSON Public  = "public" 
   toJSON Private = "private" 
instance FromJSON Visibility where

instance Show Visibility where
  show Public = "public"
  show Private = "private"

readVisibility :: Text -> Maybe Visibility
readVisibility "public" = Just Public
readVisibility "private" = Just Private
readVisibility _ = Nothing


-- | location is a pair [latitude, longitude] with the coordinates on earth in decimal notation (e.g. [40.418889, 35.89389]).
data Location = Location
  { latitude :: Double
  , longitude :: Double
  } deriving (Show, Eq, Generic)

instance FromJSON Location where
  parseJSON = genericParseJSON defaultOptions
instance ToJSON Location where
  toJSON = genericToJSON defaultOptions

-- | 
data Measurement = Measurement
  { measId :: Text                  -- ^ ID of the measurement
  , measName :: Maybe Text          -- ^ name of the measurement
  , measSensingDevice :: Maybe Text -- ^ sensing platform used for the measurement, from https://github.com/Waziup/waziup-js/blob/master/src/model/SensingDevices.js
  , measQuantityKind :: Maybe Text  -- ^ quantity measured, from https://github.com/Waziup/waziup-js/blob/master/src/model/QuantityKinds.js
  , measUnit :: Maybe Text          -- ^ unit of the measurement, from https://github.com/Waziup/waziup-js/blob/master/src/model/Units.js
  , measLastValue :: Maybe MeasurementValue -- ^ last value received by the platform
  } deriving (Show, Eq, Generic)

instance FromJSON Measurement where
  parseJSON = genericParseJSON $ aesonDrop 4 snakeCase 
instance ToJSON Measurement where
  toJSON = genericToJSON $ aesonDrop 4 snakeCase


data HistoricalValue = HistoricalValue
  { historicalValueId :: Text -- ^ UUID of the sensor
  , historicalValueAttribute'Underscoreid :: Text -- ^ UUID of the measurement
  , historicalValueDatapoint :: MeasurementValue -- ^ 
  } deriving (Show, Eq, Generic)

instance FromJSON HistoricalValue where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "historicalValue")
instance ToJSON HistoricalValue where
  toJSON = genericToJSON (removeFieldLabelPrefix False "historicalValue")
-- | 
data MeasurementValue = MeasurementValue
  { measValue :: Value           -- ^ value of the measurement
  , measTimestamp :: Maybe UTCTime    -- ^ time of the measurement
  , measDateReceived :: Maybe UTCTime -- ^ time at which the measurement has been received on the Cloud
  } deriving (Show, Eq, Generic)

instance FromJSON MeasurementValue where
  parseJSON = genericParseJSON $ aesonDrop 4 snakeCase
instance ToJSON MeasurementValue where
  toJSON = genericToJSON $ aesonDrop 4 snakeCase

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
