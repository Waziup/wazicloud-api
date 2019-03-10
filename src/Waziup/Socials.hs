{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Waziup.Socials where

import           Waziup.Types
import           Waziup.Utils
import           Waziup.Devices hiding (info, warn, debug, err)
import           Control.Monad
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
import           Web.Twitter.Conduit hiding (map)
import           Web.Twitter.Conduit.Api
import           Web.Twitter.Types as TWT
import           Network.Wreq as W
import           Control.Lens hiding ((.=))
import           GHC.Generics (Generic)

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
postSocialMessage tok msg = case socChannel msg of
  Twitter -> do
    liftIO $ postTwitter msg 
    return undefined 
  SMS -> undefined 
  Voice -> undefined

tokens :: OAuth
tokens = twitterOAuth
    { oauthConsumerKey = "QhmtxbYoNobspUn1af8fz3KYy"
    , oauthConsumerSecret = "tLcIQZNqlew5fmSTzOhK298J22kHeNxX8w7SA4XVq0AU8L2xTZ"
    }

credential :: Credential
credential = Credential
    [ ("oauth_token", "1054663210824056832-jeXkixm5rnRQFhSc4ekAXlTCqi6UTJ")
    , ("oauth_token_secret", "3CrGPwzNpsLre60kXzs5bowM1Soyxh63FkL358brgbEzz")
    ]

twInfo :: TWInfo
twInfo = def
    { twToken = def { twOAuth = tokens, twCredential = credential }
    , twProxy = Nothing
    }

postTwitter :: SocialMessage -> IO ()
postTwitter msg = do
  mgr <- newManager tlsManagerSettings
  users <- call twInfo mgr $ usersLookup $ ScreenNameListParam ["corentindupont2"]
  let usrId = TWT.userId $ L.head users
  call twInfo mgr $ directMessagesNew (UserIdParam usrId) "Hello DM" 
  return ()


postSMS :: SocialMessage -> Waziup ()
postSMS (SocialMessage _ _ _ txt) = smsPost $ PlivoSMS undefined undefined txt


postSocialMessageBatch :: Maybe Token -> SocialMessageBatch -> Waziup NoContent
postSocialMessageBatch sb = undefined


getSocialMessage :: Maybe Token -> SocialMessageId -> Waziup SocialMessage
getSocialMessage id = undefined


deleteSocialMessage :: Maybe Token -> SocialMessageId -> Waziup NoContent
deleteSocialMessage id = undefined

data PlivoSMS = PlivoSMS {
  smsSrc :: Text,
  smsDst :: Text,
  smsText :: Text} deriving (Show, Eq, Generic)

instance ToJSON PlivoSMS

smsPost :: PlivoSMS -> Waziup ()
smsPost dat = do 
  let opts = W.defaults & W.auth ?~ W.basicAuth "MAMDA5ZDJIMDM1NZVMZD" "NzRlNWJiNmU2MmFjYWJlODhlNTk3MTkyZGEzNzIy"
  let path = "https://api.plivo.com/v1/Account/" ++ "MAMDA5ZDJIMDM1NZVMZD" ++ "/Message/"
  info $ "Issuing Plivo POST " ++ (show path) 
  debug $ "  data: " ++ (show dat) 
  debug $ "  headers: " ++ (show $ opts ^. W.headers) 
  liftIO $ W.postWith opts path (toJSON dat)
  return ()
  --case eRes of 
  --  Right res -> return ()
  --  Left err -> do
  --    warn $ "Plivo HTTP Error: " ++ (show err)
  --    throwError err404 {errBody = "Plivo error"}

-- Logging
warn, info, debug, err :: (MonadIO m) => String -> m ()
debug s = liftIO $ debugM   "Socials" s
info  s = liftIO $ infoM    "Socials" s
warn  s = liftIO $ warningM "Socials" s
err   s = liftIO $ errorM   "Socials" s

