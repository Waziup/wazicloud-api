{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Keycloak.Types where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Aeson.Casing
import           Data.Text hiding (head, tail, map, toLower)
import           Data.Text.Encoding
import           Data.Monoid
import           Data.Maybe
import           Data.Aeson.BetterErrors as AB
import qualified Data.ByteString as BS
import           Data.Word8 (isSpace, _colon, toLower)
import           Control.Monad.Except (ExceptT)
import           Control.Monad.Reader as R
import           Control.Lens hiding ((.=))
import           GHC.Generics (Generic)
import           Web.HttpApiData (FromHttpApiData(..), ToHttpApiData(..))
import           Network.HTTP.Client as HC hiding (responseBody)


----------------------
-- * Keycloak Monad --
----------------------

type Keycloak a = ReaderT KCConfig (ExceptT KCError IO) a

data KCError = HTTPError HttpException  -- ^ Keycloak returned an HTTP error.
             | ParseError Text          -- ^ Failed when parsing the response
             | EmptyError               -- ^ Empty error to serve as a zero element for Monoid.

data KCConfig = KCConfig {
  _baseUrl       :: Text,
  _realm         :: Text,
  _clientId      :: Text,
  _clientSecret  :: Text,
  _adminLogin    :: Username,
  _adminPassword :: Password,
  _guestLogin    :: Username,
  _guestPassword :: Password} deriving (Eq, Show)

defaultKCConfig :: KCConfig
defaultKCConfig = KCConfig {
  _baseUrl       = "http://localhost:8080/auth",
  _realm         = "waziup",
  _clientId      = "api-server",
  _clientSecret  = "4e9dcb80-efcd-484c-b3d7-1e95a0096ac0",
  _adminLogin    = "cdupont",
  _adminPassword = "password",
  _guestLogin    = "guest",
  _guestPassword = "guest"}

type Path = Text


-------------
-- * Token --
-------------

newtype Token = Token {unToken :: BS.ByteString} deriving (Eq, Show, Generic)

instance FromJSON Token where
  parseJSON (Object v) = do
    t <- v .: "access_token"
    return $ Token $ encodeUtf8 t 

instance FromHttpApiData Token where
  parseQueryParam = parseHeader . encodeUtf8
  parseHeader (extractBearerAuth -> Just tok) = Right $ Token tok
  parseHeader _ = Left "cannot extract auth Bearer"

extractBearerAuth :: BS.ByteString -> Maybe BS.ByteString
extractBearerAuth bs =
    let (x, y) = BS.break isSpace bs
    in if BS.map toLower x == "bearer"
        then Just $ BS.dropWhile isSpace y
        else Nothing

instance ToHttpApiData Token where
  toQueryParam (Token token) = "Bearer " <> (decodeUtf8 token)
  
data TokenDec = TokenDec {
  jti :: Text,
  exp :: Int,
  nbf :: Int,
  iat :: Int,
  iss :: Text,
  aud :: Text,
  sub :: Text,
  typ :: Text,
  azp :: Text,
  authTime :: Int,
  sessionState :: Text,
  acr :: Text,
  allowedOrigins :: Value,
  realmAccess :: Value,
  ressourceAccess :: Value,
  scope :: Text,
  name :: Text,
  preferredUsername :: Text,
  givenName :: Text,
  familyName :: Text,
  email :: Text
  } deriving (Generic, Show)

--instance FromJSON TokenDec where
--  parseJSON = genericParseJSON $ aesonDrop 3 camelCase 
--instance ToJSON TokenDec where
--  toJSON = genericToJSON $ (aesonDrop 3 camelCase) {omitNothingFields = True}

parseTokenDec :: Parse e TokenDec
parseTokenDec = TokenDec <$>
    AB.key "jti" asText <*>
    AB.key "exp" asIntegral <*>
    AB.key "nbf" asIntegral <*>
    AB.key "iat" asIntegral <*>
    AB.key "iss" asText <*>
    AB.key "aud" asText <*>
    AB.key "sub" asText <*>
    AB.key "typ" asText <*>
    AB.key "azp" asText <*>
    AB.key "auth_time" asIntegral <*>
    AB.key "session_state" asText <*>
    AB.key "acr" asText <*>
    AB.key "allowed-origins" asValue <*>
    AB.key "realm_access" asValue <*>
    AB.key "resource_access" asValue <*>
    AB.key "scope" asText <*>
    AB.key "name" asText <*>
    AB.key "preferred_username" asText <*>
    AB.key "given_name" asText <*>
    AB.key "family_name" asText <*>
    AB.key "email" asText

------------------
-- * Permission --
------------------

type ScopeId = Text
type ScopeName = Text
type Scope = Text 

data Permission = Permission 
  { rsname :: ResourceName,
    rsid   :: ResourceId,
    scopes :: [Scope]
  } deriving (Generic, Show)

instance ToJSON Permission where
  toJSON = genericToJSON defaultOptions {omitNothingFields = True}

instance FromJSON Permission where
  parseJSON = genericParseJSON defaultOptions

--parsePermission :: Parse e Permission
--parsePermission = do
--    rsname  <- AB.key "rsname" asText
--    rsid    <- AB.key "rsid" asText
--    scopes  <- AB.keyMay "scopes" (eachInArray asText) 
--    return $ Permission rsname (ResourceId rsid) (if (isJust scopes) then (fromJust scopes) else [])

type Username = Text
type Password = Text


-------------
-- * Owner --
-------------

data Owner = Owner {
  ownId   :: Maybe Text,
  ownName :: Username
  } deriving (Generic, Show)

instance FromJSON Owner where
  parseJSON = genericParseJSON $ aesonDrop 3 snakeCase 

instance ToJSON Owner where
  toJSON = genericToJSON $ (aesonDrop 3 snakeCase) {omitNothingFields = True}


----------------
-- * Resource --
----------------

type ResourceName = Text

newtype ResourceId = ResourceId {unResId :: Text} deriving (Show, Eq, Generic)

-- JSON instances
instance ToJSON ResourceId where
  toJSON = genericToJSON (defaultOptions {unwrapUnaryRecords = True})

instance FromJSON ResourceId where
  parseJSON = genericParseJSON (defaultOptions {unwrapUnaryRecords = True})

data Resource = Resource {
     resId      :: Maybe ResourceId,
     resName    :: ResourceName,
     resType    :: Maybe Text,
     resUris    :: [Text],
     resScopes  :: [Scope],
     resOwner   :: Owner,
     resOwnerManagedAccess :: Bool,
     resAttributes :: [Attribute]
  } deriving (Generic, Show)

instance FromJSON Resource where
  parseJSON (Object v) = do
    rId     <- v .:? "_id"
    rName   <- v .: "name"
    rType   <- v .:? "type"
    rUris   <- v .: "uris"
    rScopes <- v .: "scopes"
    rOwn    <- v .: "owner"
    rOMA    <- v .: "ownerManagedAccess"
    rAtt    <- v .: "attributes"
    return $ Resource rId rName rType rUris rScopes rOwn rOMA rAtt

instance ToJSON Resource where
  toJSON (Resource id name typ uris scopes own uma attrs) =
    object ["name"               .= toJSON name,
            "uris"               .= toJSON uris,
            "scopes"             .= toJSON scopes,
            "owner"              .= toJSON own,
            "ownerManagedAccess" .= toJSON uma,
            "attributes"         .= object (map (\(Attribute name vals) -> name .= toJSON vals) attrs)]

data Attribute = Attribute {
  attName   :: Text,
  attValues :: [Text]
  } deriving (Generic, Show)

instance FromJSON Attribute where
  parseJSON = genericParseJSON $ aesonDrop 3 camelCase 

instance ToJSON Attribute where
  toJSON (Attribute name vals) = object [name .= toJSON vals] 



makeLenses ''KCConfig
