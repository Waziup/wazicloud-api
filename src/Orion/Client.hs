{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Orion.Client where

import           Network.Wreq as W
import           Network.Wreq.Types
import           Network.HTTP.Client (HttpException)
import           Network.HTTP.Types.Method
import           Network.HTTP.Types
import           Data.Aeson as JSON hiding (Options)
import           Data.Aeson.BetterErrors as AB
import           Data.Aeson.Casing
import           Data.Text hiding (head, tail, find, map, filter)
import           Data.Text.Encoding as TE
import           Data.Maybe
import           Data.Aeson.BetterErrors.Internal
import           Data.Time.ISO8601
import           Data.Foldable as F
import           Data.Scientific
import           Data.Monoid
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BSL
import           Data.String.Conversions
import           Control.Lens hiding ((.=))
import           Control.Monad.Reader
import           Control.Monad.Except (ExceptT, throwError, MonadError, catchError)
import           Control.Exception hiding (try)
import qualified Control.Monad.Catch as C
import           Orion.Types
import           Keycloak.Types (ResourceId(..))
import           System.Log.Logger
import           GHC.Generics (Generic)
import           Waziup.Types
import           Debug.Trace

-- * Orion REST interface

getEntities :: Maybe Text -> Orion [Entity]
getEntities mq = do
  let qq = case mq of
       Just q -> [("q", Just $ encodeUtf8 q)]
       Nothing -> []
  let (query :: Query) = qq ++ [("limit", Just $ encodeUtf8 "1000")]
  orionGet (decodeUtf8 $ "/v2/entities" <> (renderQuery True query))  (eachInArray parseEntity)

postEntity :: Entity -> Orion ()
postEntity e = do
  debug $ convertString $ "Entity: " <> (JSON.encode e)
  orionPost "/v2/entities" (toJSON e)

getEntity :: EntityId -> Orion Entity
getEntity (EntityId eid) = orionGet ("/v2/entities/" <> eid) parseEntity

deleteEntity :: EntityId -> Orion ()
deleteEntity (EntityId eid) = orionDelete ("/v2/entities/" <> eid)

postAttribute :: EntityId -> Attribute -> Orion ()
postAttribute (EntityId eid) att = do
  debug $ "Post attributen: " <> (convertString $ JSON.encode att)
  orionPost ("/v2/entities/" <> eid <> "/attrs") (toJSON att)

postTextAttributeOrion :: EntityId -> AttributeId -> Text -> Orion ()
postTextAttributeOrion (EntityId eid) attId val = do
  debug $ convertString $ "put Sensor attribute in Orion: " <> val
  orionPost ("/v2/entities/" <> eid <> "/attrs") (toJSON $ getSimpleAttr attId val)

deleteAttribute :: EntityId -> AttributeId -> Orion ()
deleteAttribute (EntityId eid) (AttributeId attId) = do
  debug $ "Delete attribute"
  orionDelete ("/v2/entities/" <> eid <> "/attrs/" <> attId)

-- * Requests to Orion.

-- Get Orion URI and options
getOrionDetails :: Path -> Orion (String, Options)
getOrionDetails path = do
  orionOpts@(OrionConfig baseUrl service) <- ask 
  let opts = defaults &
       header "Fiware-Service" .~ [convertString service] &
       param  "attrs"          .~ ["dateModified,dateCreated,*"] &
       param  "metadata"       .~ ["dateModified,dateCreated,*"] 
  let url = (unpack $ baseUrl <> path) 
  return (url, opts)

orionGet :: (Show b) => Path -> Parse Text b -> Orion b
orionGet path parser = do 
  (url, opts) <- getOrionDetails path 
  info $ "Issuing ORION GET with url: " ++ (show url) 
  debug $ "  headers: " ++ (show $ opts ^. W.headers) 
  eRes <- C.try $ liftIO $ W.getWith opts url
  case eRes of 
    Right res -> do
      let body = fromJust $ res ^? responseBody
      case AB.parse parser body of
        Right ret -> do
          debug $ "Orion result: " ++ (show ret)
          return ret
        Left err2 -> do
          err $ "Orion parse error: " ++ (show err2)
          throwError $ ParseError $ pack (show err2)
    Left err -> do
      warn $ "Orion HTTP Error: " ++ (show err)
      throwError $ HTTPError err

orionPost :: (Postable dat, Show dat) => Path -> dat -> Orion ()
orionPost path dat = do 
  (url, opts) <- getOrionDetails path 
  info $ "Issuing ORION POST with url: " ++ (show url) 
  debug $ "  data: " ++ (show dat) 
  debug $ "  headers: " ++ (show $ opts ^. W.headers) 
  eRes <- C.try $ liftIO $ W.postWith opts url dat
  case eRes of 
    Right res -> return ()
    Left err -> do
      warn $ "Orion HTTP Error: " ++ (show err)
      throwError $ HTTPError err

orionDelete :: Path -> Orion ()
orionDelete path = do 
  (url, opts) <- getOrionDetails path 
  info $ "Issuing ORION DELETE with url: " ++ (show url) 
  debug $ "  headers: " ++ (show $ opts ^. W.headers) 
  eRes <- C.try $ liftIO $ W.deleteWith opts url
  case eRes of 
    Right res -> return ()
    Left err -> do
      warn $ "Orion HTTP Error: " ++ (show err)
      throwError $ HTTPError err

orionPut :: (Putable dat, Show dat) => Path -> dat -> Orion ()
orionPut path dat = do 
  (url, opts) <- getOrionDetails path 
  info $ "Issuing ORION PUT with url: " ++ (show url) 
  debug $ "  data: " ++ (show dat) 
  debug $ "  headers: " ++ (show $ opts ^. W.headers) 
  eRes <- C.try $ liftIO $ W.putWith opts url dat
  case eRes of 
    Right res -> return ()
    Left err -> do
      warn $ "Orion HTTP Error: " ++ (show err)
      throwError $ HTTPError err

-- * Helpers

fromSimpleAttribute :: AttributeId -> [Attribute] -> Maybe Text
fromSimpleAttribute attId attrs = do
   (Attribute _ _ mval _) <- find (\(Attribute attId' _ _ _) -> attId' == attId) attrs
   val <- mval
   getString val

fromSimpleMetadata :: MetadataId -> [Metadata] -> Maybe Text
fromSimpleMetadata mid mets = do
   (Metadata _ _ mval) <- find (\(Metadata mid' _ _) -> mid' == mid) mets
   val <- mval
   getString val

getString :: Value -> Maybe Text
getString (String s) = Just s
getString _ = Nothing

getSimpleAttr :: AttributeId -> Text -> Attribute
getSimpleAttr attId val = Attribute attId "String" (Just $ toJSON val) []

getTextMetadata :: MetadataId -> Text -> Metadata
getTextMetadata metId val = Metadata metId (Just "String") (Just $ toJSON val)

debug, warn, info, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Orion" s
info s  = liftIO $ infoM    "Orion" s
warn s  = liftIO $ warningM "Orion" s
err s   = liftIO $ errorM   "Orion" s

try :: MonadError a m => m b -> m (Either a b)
try act = catchError (Right <$> act) (return . Left)

