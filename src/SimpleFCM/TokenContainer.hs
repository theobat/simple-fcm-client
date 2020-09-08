{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module SimpleFCM.TokenContainer where

import Protolude
import Data.String (String)
import Network.Google.OAuth2.JWT (SignedJWT, fromPEMString, getSignedJWT)
import qualified Network.Wreq as NW
import Data.Aeson (decode', FromJSON, ToJSON(toJSON))
import Control.Lens ( (&), (^.), (.~) )
import Data.Text (pack)
import Data.String (String, IsString(fromString))

type Email = Text

newtype GoogleAccessToken = GoogleAccessToken Text
  deriving newtype (Eq, Show, FromJSON)
newtype GoogleMainToken = GoogleMainToken SignedJWT
  deriving newtype (Eq, Show)
instance ToJSON GoogleMainToken where
  toJSON (GoogleMainToken value) = toJSON @Text (show value) 

-- | The authorization asked for this token.
-- see the end paragraph in https://firebase.google.com/docs/cloud-messaging/migrate-v1#use-credentials-to-mint-access-tokens
fcmScope :: [Text]
fcmScope = ["https://www.googleapis.com/auth/firebase.messaging", "https://www.googleapis.com/auth/cloud-platform"]

expirationInSeconds :: Maybe Int
expirationInSeconds = Just 3600

newtype ProjectId = ProjectId Text
  deriving newtype (Eq, Show)

instance IsString ProjectId where
  fromString = ProjectId . pack  

data TokenSettings = TokenSettings {
    serviceClientEmail :: Email,
    rawPrivateKey :: String,
    projectId :: ProjectId
  }
  deriving (Eq, Show)

-- This only provides a JWT token provider.
getToken :: TokenSettings -> IO (Either String SignedJWT)
getToken settings = do
  privateKey <- fromPEMString $ rawPrivateKey settings
  getSignedJWT email (Just email) fcmScope expirationInSeconds privateKey
  where
    email = serviceClientEmail settings

data GoogleAccessTokenRequest = GoogleAccessTokenRequest
  { scope :: Text,
    grant_type :: Text,
    assertion :: GoogleMainToken
  } deriving (Show, Eq, Generic, ToJSON)

defaultGoogleAccessTokenRequest :: SignedJWT -> GoogleAccessTokenRequest
defaultGoogleAccessTokenRequest signedJWT = GoogleAccessTokenRequest {
    scope = "https://www.googleapis.com/auth/cloud-platform",
    grant_type = "urn:ietf:params:oauth:grant-type:jwt-bearer",
    assertion = GoogleMainToken signedJWT
  }
  
getFCMAccessToken :: SignedJWT -> IO (Either Text GoogleAccessTokenReturn)
getFCMAccessToken signedJWT = do
  let accessTokenInput = defaultGoogleAccessTokenRequest signedJWT
  let opts = NW.defaults
  let msgVal=toJSON $ accessTokenInput
  resp <- NW.postWith opts "https://oauth2.googleapis.com/token" msgVal
  putStrLn ("Resp:" <> show resp::Text)
  let respStatus = resp ^. NW.responseStatus
  let respBody = resp ^. NW.responseBody
  let bodyRes = decode' @GoogleAccessTokenReturn respBody
  pure $ maybeToRight (show resp) bodyRes

data GoogleAccessTokenReturn = GoogleAccessTokenReturn {
  access_token :: GoogleAccessToken,
  expires_in :: Int,
  token_type :: Text
} deriving (Eq, Show, Generic, FromJSON)

-- | The class for token container
class GoogleTokenContainer containerT where
  setGoogleMainToken :: GoogleMainToken -> containerT -> containerT
  getGoogleMainToken :: containerT -> GoogleMainToken
  setGoogleAccessToken :: GoogleAccessToken -> containerT -> containerT
  getGoogleAccessToken :: containerT -> GoogleAccessToken

tokenUpdaterThread :: GoogleTokenContainer containerT => Maybe Int -> TokenSettings -> MVar containerT -> IO ()
tokenUpdaterThread delay settings input = const () <$> (forkIO $ tokenTimer delay settings input)

tokenTimer :: GoogleTokenContainer containerT => Maybe Int -> TokenSettings -> MVar containerT -> IO ()
tokenTimer delay settings input = do
  -- print "tik"
  threadDelay (fromMaybe (1000000 * 60 * 55) delay)
  -- print "tok"
  modifyMVar_ input updater
  tokenTimer delay settings input
  where
    updater !prevContainer = do
      setRes <- runExceptT $ generateBothToken settings
      -- print "modified MVAR"
      case setRes of
        Left err -> print err >> pure prevContainer
        Right !(mainToken, accessToken) -> pure (setGoogleMainToken mainToken . setGoogleAccessToken accessToken $ prevContainer)
      
generateBothToken :: TokenSettings -> ExceptT Text IO (GoogleMainToken, GoogleAccessToken)
generateBothToken settings = (fmap . fmap) access_token $ generateBothQueries settings

generateBothQueries :: TokenSettings -> ExceptT Text IO (GoogleMainToken, GoogleAccessTokenReturn)
generateBothQueries settings = do
  mainToken <- ExceptT $ first pack <$> getToken settings
  accessToken <- ExceptT $ (getFCMAccessToken mainToken)
  pure (GoogleMainToken mainToken, accessToken)