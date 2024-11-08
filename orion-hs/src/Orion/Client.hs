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
import           Data.Text  as T hiding (head, tail, find, map, filter, singleton, empty)
import           Data.Text.Encoding as TE
import           Data.Maybe
import           Data.Aeson.BetterErrors.Internal
import           Data.Time
import           Data.Time.ISO8601
import           Data.Foldable as F
import           Data.Monoid
import           Data.Map hiding (lookup, drop)
import qualified Data.HashMap.Strict as H
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import           Data.String.Conversions
import           Control.Lens hiding ((.=))
import           Control.Monad.Reader
import           Control.Monad.Except (ExceptT, throwError, MonadError, catchError)
import           Control.Exception hiding (try)
import qualified Control.Monad.Catch as C
import           Control.Monad.Extra
import           Orion.Types
import           System.Log.Logger
import           GHC.Generics (Generic)
import           Debug.Trace
import           Safe
import Data.String.Conversions.Monomorphic (toString)

-- * Entities

getEntities :: Maybe Text -> Maybe EntityType -> Orion [Entity]
getEntities mq mtyp = do
  (ents, tot) <- getEntities' mq mtyp 1000 0
  info $ "total:" ++ (show tot)
  -- If the total number of entities is more than 1000, we need to issue additional requests (Orion limit is 1000 per request)
  let n = tot `div` 1000
  ents' <- concatMapM (\n -> getEntities' mq mtyp 1000 (n * 1000) >>= return . fst) [1..n]
  return $ ents <> ents'

getEntities' :: Maybe Text -> Maybe EntityType -> Int -> Int -> Orion ([Entity], Int)
getEntities' mq mtyp limit offset = do
  let qq = case mq of
       Just q -> [("q", Just $ encodeUtf8 q)]
       Nothing -> []
  let typ = case mtyp of
       Just t -> [("type", Just $ encodeUtf8 t)]
       Nothing -> []
  let (query :: Query) = typ ++ qq ++ [("limit",   Just $ convertString (show limit)), 
                                       ("offset",  Just $ convertString (show offset)),
                                       ("options", Just $ encodeUtf8 "count")]
  (body, headers) <- orionGetHeaders (decodeUtf8 $ "/v2/entities" <> (renderQuery True query))
  case eitherDecode body of
    Right ret -> do
      case lookup "Fiware-Total-Count" headers of
        Just c -> return (ret, readNote "getEntities" $ convertString c)
        Nothing -> error "No headers"
    Left (e :: String) -> do
      debug $ "Orion parse error: " ++ (show e) 
      throwError $ ParseError $ pack (show e)

postEntity :: Entity -> Orion SubId 
postEntity e = do
  debug $ convertString $ "Entity: " <> (JSON.encode e)
  res <- orionPost "/v2/entities" (toJSON e)
  return $ SubId $ convertString res

getEntity :: EntityId -> Maybe EntityType -> Orion Entity
getEntity (EntityId eid) mtyp = do
  let typ = case mtyp of
       Just t -> [("type", Just $ encodeUtf8 t)]
       Nothing -> []
  body <- orionGet ("/v2/entities/" <> eid <> (convertString $ renderQuery True typ))
  case eitherDecode body of
    Right ret -> do
      return ret
    Left (e :: String) -> do
      debug $ "Orion parse error: " ++ (show e) 
      throwError $ ParseError $ pack (show e)

deleteEntity :: EntityId -> Maybe EntityType -> Orion ()
deleteEntity (EntityId eid) mtyp = do
  let typ = case mtyp of
       Just t -> [("type", Just $ encodeUtf8 t)]
       Nothing -> []
  orionDelete ("/v2/entities/" <> eid <> (convertString $ renderQuery True typ))

postAttribute :: EntityId -> Maybe EntityType -> (AttributeId, Attribute) -> Orion ()
postAttribute (EntityId eid) mtyp (attId, att) = do
  debug $ "Post attribute: " <> (convertString $ JSON.encode att)
  let typ = case mtyp of
       Just t -> [("type", Just $ encodeUtf8 t)]
       Nothing -> []
  void $ orionPost ("/v2/entities/" <> eid <> "/attrs" <> (convertString $ renderQuery True typ)) (toJSON $ singleton attId att)

postTextAttributeOrion :: EntityId -> Maybe EntityType -> AttributeId -> Text -> Orion ()
postTextAttributeOrion (EntityId eid) mtyp attId val = do
  debug $ convertString $ "put attribute in Orion: " <> val
  let typ = case mtyp of
       Just t -> [("type", Just $ encodeUtf8 t)]
       Nothing -> []
  void $ orionPost ("/v2/entities/" <> eid <> "/attrs" <> (convertString $ renderQuery True typ)) (toJSON $ fromList [getSimpleAttr attId val])

postValueAttributeOrion :: EntityId -> Maybe EntityType -> AttributeId -> Value -> Orion ()
postValueAttributeOrion (EntityId eid) mtyp attId val = do
  debug $ convertString $ "put attribute in Orion: " <> show val
  let typ = case mtyp of
       Just t -> [("type", Just $ encodeUtf8 t)]
       Nothing -> []
  void $ orionPost ("/v2/entities/" <> eid <> "/attrs" <> (convertString $ renderQuery True typ)) (toJSON $ fromList [getValueAttr attId val])


deleteAttribute :: EntityId -> Maybe EntityType -> AttributeId -> Orion ()
deleteAttribute (EntityId eid) mtyp (AttributeId attId) = do
  debug $ "Delete attribute"
  let typ = case mtyp of
       Just t -> [("type", Just $ encodeUtf8 t)]
       Nothing -> []
  orionDelete ("/v2/entities/" <> eid <> "/attrs/" <> attId <> (convertString $ renderQuery True typ))


-- * Subscriptions

getSubs :: Orion [Subscription]
getSubs = do 
  debug $ "Get subscriptions"
  body <- orionGet ("/v2/subscriptions?limit=1000")
  case eitherDecode body of
    Right ret -> do
      return ret
    Left (err2 :: String) -> do
      debug $ "Orion parse error: " ++ (show err2) 
      throwError $ ParseError $ pack (show err2)

postSub :: Subscription -> Orion SubId 
postSub e = do
  debug $ convertString $ "PostSubscription: " <> (JSON.encode e)
  res <- orionPost "/v2/subscriptions" (toJSON e)
  debug $ "Orion resp: " ++ (show res)
  return $ SubId $ convertString res

getSub :: SubId -> Orion Subscription
getSub (SubId eid) = do
  body <- orionGet ("/v2/subscriptions/" <> eid)
  case eitherDecode body of
    Right ret -> do
      return ret
    Left (err2 :: String) -> do
      debug $ "Orion parse error: " ++ (show err2) 
      throwError $ ParseError $ pack (show err2)

patchSub :: SubId -> Subscription -> Orion () 
patchSub (SubId eid) e = do
  debug $ convertString $ "PatchSubscription: " <> (JSON.encode e)
  res <- orionPatch ("/v2/subscriptions/" <> eid) (toJSON e)
  debug $ "Orion resp: " ++ (show res)
  return () 

deleteSub :: SubId -> Orion ()
deleteSub (SubId sid) = orionDelete ("/v2/subscriptions/" <> sid)

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

orionGet :: Path -> Orion BL.ByteString 
orionGet path = do 
  (url, opts) <- getOrionDetails path 
  info $ "Issuing ORION GET with url: " ++ (show url) 
  debug $ "  headers: " ++ (show $ opts ^. W.headers) 
  eRes <- C.try $ liftIO $ W.getWith opts url
  case eRes of 
    Right res -> do
      return $ fromJust $ res ^? responseBody
    Left err -> do
      warn $ "Orion HTTP error: " ++ (show err)
      throwError $ HTTPError err

orionGetHeaders :: Path -> Orion (BL.ByteString, ResponseHeaders)
orionGetHeaders path = do 
  (url, opts) <- getOrionDetails path 
  info $ "Issuing ORION GET with url: " ++ (show url) 
  debug $ "  headers: " ++ (show $ opts ^. W.headers) 
  eRes <- C.try $ liftIO $ W.getWith opts url
  case eRes of 
    Right res -> do
      return (fromJust $ res ^? responseBody, fromJust $ res ^? responseHeaders)
    Left err -> do
      warn $ "Orion HTTP error: " ++ (show err)
      throwError $ HTTPError err

orionPost :: (Postable dat, Show dat) => Path -> dat -> Orion Text 
orionPost path dat = do 
  (url, opts) <- getOrionDetails path 
  info $ "Issuing ORION POST with url: " ++ (show url) 
  debug $ "  data: " ++ (show dat) 
  debug $ "  headers: " ++ (show $ opts ^. W.headers) 
  eRes <- C.try $ liftIO $ W.postWith opts url dat
  debug $ " resp: " ++ (show eRes) 
  case eRes of 
    Right res -> do
     let headers = fromJust $ res ^? responseHeaders
     return $ T.drop 18 $ convertString $ fromJust $ lookup "Location" headers
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

orionPatch :: (Postable dat, Show dat) => Path -> dat -> Orion ()
orionPatch path dat = do 
  (url, opts) <- getOrionDetails path 
  info $ "Issuing ORION PATCH with url: " ++ (show url) 
  debug $ "  data: " ++ (show dat) 
  debug $ "  headers: " ++ (show $ opts ^. W.headers) 
  eRes <- C.try $ liftIO $ W.customPayloadMethodWith "PATCH" opts url dat
  case eRes of 
    Right res -> return ()
    Left err -> do
      warn $ "Orion HTTP Error: " ++ (show err)
      throwError $ HTTPError err

-- * Helpers
fromBoolAttribute :: AttributeId -> Map AttributeId Attribute -> Maybe Bool
fromBoolAttribute attId attrs = do
  (Attribute _ mval _) <- attrs !? attId
  val <- mval
  case fromJSON val of
    Success a -> return a
    Error _ -> Nothing

fromSimpleAttribute :: AttributeId -> Map AttributeId Attribute -> Maybe Text
fromSimpleAttribute attId attrs = do
  (Attribute _ mval _) <- attrs !? attId
  val <- mval
  getString val

fromValueAttribute :: AttributeId -> Map AttributeId Attribute -> Maybe Value
fromValueAttribute attId attrs = do
  (Attribute _ mval _) <- attrs !? attId
  mval

fromSimpleMetadata :: MetadataId -> Map MetadataId Metadata -> Maybe Text
fromSimpleMetadata mid mets = do
  (Metadata _ mval) <- mets !? mid
  val <- mval
  getString val

fromValueMetadata :: MetadataId -> Map MetadataId Metadata -> Maybe Value
fromValueMetadata mid mets = do
  (Metadata _ mval) <- mets !? mid
  mval

getString :: Value -> Maybe Text
getString (String s) = Just s
getString _ = Nothing

getSimpleAttr :: AttributeId -> Text -> (AttributeId, Attribute)
getSimpleAttr attId val = (attId, Attribute "String" (Just $ toJSON val) empty)

getValueAttr :: AttributeId -> Value -> (AttributeId, Attribute)
getValueAttr attId val = (attId, Attribute "Value" (Just val ) empty)

getTextMetadata :: MetadataId -> Text -> (MetadataId, Metadata)
getTextMetadata metId val = (metId, Metadata (Just "String") (Just $ toJSON val))

getValueMetadata :: MetadataId -> Value ->(MetadataId, Metadata)
getValueMetadata metId val = (metId, Metadata (Just "Value") (Just val))

getTimeMetadata :: MetadataId -> UTCTime -> (MetadataId, Metadata)
getTimeMetadata metId val = (metId, (Metadata (Just "DateTime") (Just $ toJSON $ formatISO8601 val)))

debug, warn, info, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Orion" s
info s  = liftIO $ infoM    "Orion" s
warn s  = liftIO $ warningM "Orion" s
err s   = liftIO $ errorM   "Orion" s

try :: MonadError a m => m b -> m (Either a b)
try act = catchError (Right <$> act) (return . Left)

