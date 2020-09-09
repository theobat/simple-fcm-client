{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module SimpleFCM.FCMV1 where

import Control.Lens ((&), (.~), (^.))
import Data.Aeson (Object, ToJSON (toJSON))
import qualified Data.Aeson as DA
import qualified Data.Aeson.Types as DA
import Data.HashMap.Strict (fromList)
import Data.String (IsString (fromString), String)
import Data.Text (pack)
import Network.Google.OAuth2.JWT (SignedJWT, fromPEMString, getSignedJWT)
import Network.HTTP.Types.Status (statusIsSuccessful)
import qualified Network.Wreq as NW
import Protolude hiding ((&))
import SimpleFCM.TokenContainer (GoogleAccessToken, GoogleTokenContainer, TokenSettings (projectId), asBearer, getGoogleAccessToken, projectIdToURLPart)

-- | A notification is __part__ of an FCM message.
-- It's what is shown in case a message is received __in the background__ (while the app is
-- backgrounded).
-- It's not possible to send just a notification, you must use the
-- 'FCMMessage' type.
data FCMNotification = FCMNotification
  { -- | The title to render on the notification's rendering (if received in the background).
    title :: Text,
    -- | The body to render on the notification's rendering (still if received in the background).
    body :: Text
  }
  deriving (Show, Eq, Generic, ToJSON)

data FCMMessage = FCMMessage
  { notification :: Maybe FCMNotification,
    payload :: Maybe DA.Object,
    topic :: Maybe Text
    -- token :: Maybe Text
  }
  deriving (Show, Eq, Generic)

simpleNotification ::
  -- | Notification's title
  Text ->
  -- | Notification's body
  Text ->
  FCMNotification
simpleNotification titleV bodyV = FCMNotification {
    title=titleV,
    body=bodyV
  }

-- | A message with a title, a body and a topic to broadcast to.
simpleMessage ::
  -- | Message's topic (key to boradcast to).
  Text ->
  -- | Notification's title
  Text ->
  -- | Notification's body
  Text ->
  FCMMessage
simpleMessage topicV titleV bodyV =
  FCMMessage
    { notification = Just $ simpleNotification titleV bodyV,
      payload = Nothing,
      topic = Just topicV
    }

-- | Like a 'simpleMessage' but with an additional JSON payload.
-- In google's FCM spec, the key is _data_, but it's a reserved word in
payloadMessage ::
  (ToJSON payloadT) =>
  -- |  The topic to broadcast to.
  Text ->
  -- |  The notification's part of the message (title and body).
  --  If not provided a pure _data_ message will be sent anyway
  Maybe FCMNotification ->
  -- | The payload you provide.
  -- If the toJSON function does not yield an object, this payload will be ignored
  Maybe payloadT ->
  -- | The resulting message.
  FCMMessage
payloadMessage topicV notification payloadV =
  FCMMessage
    { notification = notification,
      payload = case toJSON <$> payloadV of
        Just (DA.Object obj) -> Just obj
        _ -> Nothing,
      topic = Just topicV
    }

replaceData :: String -> String
replaceData "payload" = "data"
replaceData a = a

customOptions :: DA.Options
customOptions =
  DA.defaultOptions
    { DA.fieldLabelModifier = replaceData
    }

instance DA.ToJSON FCMMessage where
  toJSON = DA.genericToJSON customOptions
  toEncoding = DA.genericToEncoding customOptions

-- >>> import SimpleFCM.TokenContainer (TokenSettings(TokenSettings))
-- >>> messageURL (TokenSettings "" "" "project-123")
-- "https://fcm.googleapis.com/v1/projects/project-123/messages:send"
messageURL :: TokenSettings -> String
messageURL settings = base <> variable <> suffix
  where
    base = "https://fcm.googleapis.com/v1/projects/"
    variable = projectIdToURLPart . projectId $ settings
    suffix = "/messages:send"

sendMessage :: GoogleTokenContainer containerT => containerT -> TokenSettings -> FCMMessage -> IO (Either Text NW.Status)
sendMessage tokenCont settings fcmMsg = do
  let token = getGoogleAccessToken tokenCont
  sendMessageWithAccessToken token settings fcmMsg

data Msg a = Msg {message :: a} deriving (Eq, Show, Generic, ToJSON)

sendMessageWithAccessToken :: GoogleAccessToken -> TokenSettings -> FCMMessage -> IO (Either Text NW.Status)
sendMessageWithAccessToken token settings fcmMsg = do
  let authHeader = asBearer token
  print authHeader
  let opts = NW.defaults & NW.header "Authorization" .~ [authHeader]
  let msgVal = DA.toJSON $ Msg fcmMsg
  print msgVal
  resp <- NW.postWith opts (messageURL settings) msgVal
  -- putStrLn ("Resp:" <> show resp::Text)
  let respStatus = resp ^. NW.responseStatus
  pure $ (if statusIsSuccessful respStatus then Right respStatus else Left (show resp))
