{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module SimpleFCM.FCMV1 where
import Protolude hiding ((&))
import Data.String (String, IsString(fromString))
import qualified Network.Wreq as NW
import qualified Data.Aeson as DA
import qualified Data.Aeson.Types as DA
import Control.Lens ( (&), (^.), (.~) )
import Network.HTTP.Types.Status (statusIsSuccessful)
import Network.Google.OAuth2.JWT (SignedJWT, fromPEMString, getSignedJWT)
import Data.Text (pack)
import Data.HashMap.Strict (fromList)
import Data.Aeson (ToJSON(toJSON))
import SimpleFCM.TokenContainer (asBearer, GoogleAccessToken, projectIdToURLPart, TokenSettings(projectId), getGoogleAccessToken, GoogleTokenContainer)

data FCMNotification = FCMNotification
  { title :: Text
  , body :: Text
  } deriving (Show, Eq, Generic, ToJSON)

data FCMMessage = FCMMessage
  { notification :: FCMNotification,
    payload :: Maybe DA.Object,
    topic :: Maybe Text,
    token :: Maybe Text
  } deriving (Show, Eq, Generic)

simpleMessage :: Text -> Text -> Text -> FCMMessage
simpleMessage topicV titleV bodyV = FCMMessage {
    notification = FCMNotification {
      title = titleV,
      body = bodyV
    },
    payload = Nothing,
    topic = Just topicV,
    token = Nothing
  }

replaceData :: String -> String
replaceData "payload" = "data"
replaceData a = a

customOptions :: DA.Options
customOptions = DA.defaultOptions
  { DA.fieldLabelModifier = replaceData
  }

instance DA.ToJSON FCMMessage where
    toJSON     = DA.genericToJSON customOptions
    toEncoding = DA.genericToEncoding customOptions

-- >>> import SimpleFCM.TokenContainer (TokenSettings(TokenSettings)) 
-- >>> messageURL (TokenSettings "" "" "project-123")
-- "https://fcm.googleapis.com/v1/projects/project-123/messages:send"
messageURL :: TokenSettings -> String
messageURL settings = base <> (projectIdToURLPart . projectId $ settings) <> suffix
  where
    base = "https://fcm.googleapis.com/v1/projects/"
    suffix = "/messages:send"

sendMessage :: GoogleTokenContainer containerT => containerT -> TokenSettings -> FCMMessage -> IO (Either NW.Status NW.Status)
sendMessage tokenCont settings fcmMsg = do
  let token = getGoogleAccessToken tokenCont
  sendMessageWithAccessToken token settings fcmMsg

data Msg a = Msg { message :: a} deriving (Eq, Show, Generic, ToJSON)

sendMessageWithAccessToken :: GoogleAccessToken -> TokenSettings -> FCMMessage -> IO (Either NW.Status NW.Status)
sendMessageWithAccessToken token settings fcmMsg = do
  let authHeader = asBearer token
  let opts = NW.defaults & NW.header "Authorization" .~ [authHeader]
      msgVal=DA.toJSON $ Msg fcmMsg
  resp <- NW.postWith opts (messageURL settings) msgVal
  putStrLn ("Resp:" <> show resp::Text)
  let respStatus = resp ^. NW.responseStatus
  pure $ (if statusIsSuccessful respStatus then Right else Left) respStatus
