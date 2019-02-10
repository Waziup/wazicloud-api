{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Waziup.Socials where

import           Waziup.Types
import           Waziup.Utils
import           Waziup.Devices hiding (info, warn, debug, err)
import           Control.Monad.Except (throwError)
import           Control.Monad.IO.Class
import           Control.Monad.Catch as C
import           Data.Maybe
import           Data.Text hiding (map, filter, foldl, any, find)
import           Data.String.Conversions
import qualified Data.List as L
import           Data.Aeson as JSON
import           Data.AesonBson
import           Data.Time
import           Data.Time.ISO8601
import           Servant
import           Keycloak as KC hiding (info, warn, debug, err, Scope) 
import           System.Log.Logger
import           Database.MongoDB as DB hiding (value)

getSocialMessages :: Maybe Token -> Waziup [SocialMessage]
getSocialMessages tok = runMongo $ do
  let filter = ["user" =: ("test" :: Text)]
  let sel = (select filter "waziup_social_msgs") 
  cur <- find sel
  docs <- rest cur
  debug $ "Got docs:" <> (show docs)
  let res = map (fromJSON . Object . aesonify) docs
  debug $ "Got messages:" <> (show res)
  case (sequence res) of
    JSON.Success a -> return a
    JSON.Error e -> do
      err $ "Error from Mongo:" ++ (show e)
      return []
                                              
postSocialMessage :: Maybe Token -> SocialMessage -> Waziup SocialMessageId
postSocialMessage msg = undefined

postSocialMessageBatch :: Maybe Token -> SocialMessageBatch -> Waziup NoContent
postSocialMessageBatch sb = undefined


getSocialMessage :: Maybe Token -> SocialMessageId -> Waziup SocialMessage
getSocialMessage id = undefined


deleteSocialMessage :: Maybe Token -> SocialMessageId -> Waziup NoContent
deleteSocialMessage id = undefined

-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Socials" s
info  s = liftIO $ infoM    "Socials" s
warn  s = liftIO $ warningM "Socials" s
err   s = liftIO $ errorM   "Socials" s

