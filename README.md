# simple-fcm-client

Google Firebase Cloud Messaging v1  relies on 2 tokens, one for getting access tokens, and access tokens.
The protocole description is available here : https://firebase.google.com/docs/cloud-messaging/http-server-ref?hl=fr

# Basic usage

To send an FCM message you have to :

- Create a type for hosting the auth information (or implement the GoogleTokenContainer class for your existing config type)

```hs
-- for instance juste like in the test folder : 
data TestTokenContainer = TestTokenContainer {
      mainToken :: GoogleMainToken,
      accessToken :: GoogleAccessToken
} deriving (Eq, Show)

instance GoogleTokenContainer TestTokenContainer where
  setGoogleMainToken a b = b{mainToken = a}
  getGoogleMainToken = mainToken
  setGoogleAccessToken a b = b{accessToken = a}
  getGoogleAccessToken = accessToken

```

- Then generate auth data, message data and send the message :
```haskell
main = do
  let settings = TokenSettings {
    serviceClientEmail = "", -- comes from your firebase project
    rawPrivateKey = "", -- comes from your firebase project
    projectId = "" -- comes from your firebase project
    }
  tokenContainerOrError <- runExceptT $ generateBothToken settings
  -- Beware the container tokens are only valid for one hour, you have to replicate
  -- the accessToken refresh every hour.
  tokenContainer <- case tokenContainerOrError of
    Left err -> panic (show err)
    Right (main_token, access_token) -> pure $ TestTokenContainer main_token access_token
  let message = simpleMessage "allUsers" "ok" "ok"
  sendMessage settings tokenContainer message
```