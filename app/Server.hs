module Main where

import Control.Concurrent.Classy.Async
import Coorish
import Data.Text (splitOn)
import Env
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger
import Relude
import Servant
import System.Environment

type WebHookAPI = Get '[PlainText] Text

app :: [Config] -> Application
app configs = serve (Proxy :: Proxy WebHookAPI) $ webhookVerify configs

webhookVerify :: [Config] -> Handler Text
webhookVerify configs = do
  result <- liftIO $ mapConcurrently forConfig configs
  return $ unlines $ mconcat result

main :: IO ()
main = do
  portString <- getEnv "COORISH_SERVER_PORT"
  let port = either (error "Unknwon port") id $ readEither portString
  putTextLn $ "Coorish server is listening on port " <> fromString portString

  defs <- splitOn ";" . fromString <$> getEnv "COORISH_SERVER_CONFIG"
  let configs = flip map defs $ \def ->
        let [k, v] = splitOn "=" def
         in Config k $ splitOn "," v

  runSettings (setPort port defaultSettings) $ logStdoutDev $ app configs
