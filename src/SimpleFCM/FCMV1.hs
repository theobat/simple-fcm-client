{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : SimpleFCM.FCMV1
-- Description : This module implements the specification described in
-- <https://firebase.google.com/docs/cloud-messaging/http-server-ref the firebase documentation >
module SimpleFCM.FCMV1 where

import Control.Lens ((&), (.~), (^.))
import Data.Aeson (ToJSON (toJSON))
import qualified Data.Aeson as DA
import Data.String (String)
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
----------- Android/Firebase
data AndroidPriority = NORMAL | HIGH deriving (Show, Eq, Generic, ToJSON)

data AndroidOption = AndroidOption {
  priority :: Maybe AndroidPriority,
  ttl :: Maybe Text
} deriving (Show, Eq, Generic, ToJSON)
defaultAndroidOption :: AndroidOption
defaultAndroidOption = AndroidOption {
  priority = Nothing,
  ttl = Nothing
}
----------- IOS/APNS
-- | Limited between 0 to 10.
data ApnsPriority = ApnsPriority Int deriving (Show, Eq, Generic)
instance ToJSON ApnsPriority where
  toJSON (ApnsPriority v) = toJSON @Text . show $ v

data ApnsHeaders = ApnsHeaders {
  apnsPriority :: Maybe ApnsPriority
} deriving (Show, Eq, Generic)
instance ToJSON ApnsHeaders where
  toJSON = DA.genericToJSON apnsJSONOption
  toEncoding = DA.genericToEncoding apnsJSONOption
apnsJSONOption = DA.defaultOptions
    { DA.fieldLabelModifier = replaceApnsHeader,
      DA.omitNothingFields = True
    }
defaultApnsHeaders :: ApnsHeaders
defaultApnsHeaders = ApnsHeaders {
  apnsPriority = Nothing
}
data ApnsOption = ApnsOption {
  headers :: Maybe ApnsHeaders
} deriving (Show, Eq, Generic, ToJSON)
defaultApnsOption :: ApnsOption
defaultApnsOption = ApnsOption {
  headers = Nothing
}

data FCMMessage = FCMMessage
  { notification :: Maybe FCMNotification,
    payload :: Maybe DA.Object,
    topic :: Maybe Text,
    android :: Maybe AndroidOption,
    apns :: Maybe ApnsOption
  }
  deriving (Show, Eq, Generic)

simpleNotification ::
  -- | Notification's title
  Text ->
  -- | Notification's body
  Text ->
  FCMNotification
simpleNotification titleV bodyV =
  FCMNotification
    { title = titleV,
      body = bodyV
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
      topic = Just topicV,
      android = Nothing,
      apns = Nothing
    }

-- | Basically sets the importance of your message for both android and IOS.
-- The importance has an impact on your message delivery's swiftness. 
-- See the importance of a message in FCM:
-- <https://firebase.google.com/docs/cloud-messaging/concept-options#setting-the-priority-of-a-message here>
setImportance :: AndroidPriority -> ApnsPriority -> FCMMessage -> FCMMessage
setImportance androidVal apnsVal input = input {
      android = case android input of
        Just a -> Just a{ priority = Just androidVal}
        Nothing -> Just defaultAndroidOption{ priority = Just androidVal }
      ,
      apns = case apns input of
        Just a -> Just a{ headers = Just defaultApnsHeaders{ apnsPriority = Just apnsVal}}
        Nothing -> Just defaultApnsOption{ headers = Just defaultApnsHeaders{ apnsPriority = Just apnsVal}}
  }

-- | Same as 'setImportance' but through a simplified binary notion (which happen to be the android one)
-- basically sets the APNS importance to 10 if the given android one is at HIGH, and to 5 otherwise.
setImportanceSimple :: AndroidPriority -> FCMMessage -> FCMMessage
setImportanceSimple HIGH input  = setImportance HIGH (ApnsPriority 10) input
setImportanceSimple NORMAL input  = setImportance NORMAL (ApnsPriority 5) input

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
      topic = Just topicV,
      android = Nothing,
      apns = Nothing
    }

-- | Replaces haskell keywords or haskell's incorrect syntax for 
-- JSON messages.
-- Example:
-- - @"payload"@ is used instead of @"data"@ (and subsequently replaced here for sending the JSON)
-- because data is a protected keyword in haskell. 
replaceData :: String -> String
replaceData "payload" = "data"
replaceData a = a

replaceApnsHeader :: String -> String
replaceApnsHeader "apnsPriority" = "apns-priority"
replaceApnsHeader a = a  

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
