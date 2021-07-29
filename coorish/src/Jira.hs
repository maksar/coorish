{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Jira (projectCards, JiraConfig, ProjectCard (key, projectName, people), Person (displayName)) where

import Data.Aeson
  ( FromJSON (parseJSON),
    withObject,
    (.:),
    (.:?),
  )
import Data.Aeson.Types (Parser, Value (..))
import Data.ByteString.Base64 (encode)
import Data.CaseInsensitive (mk)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Data.Text (replace)
import Env (prefix)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Relude
import Relude.Extra
import Servant.API
  ( JSON,
    QueryParam',
    Required,
    StdMethod (GET),
    Strict,
    Verb,
    type (:<|>) (..),
    type (:>),
  )
import Servant.Client
  ( ClientEnv (makeClientRequest),
    ClientM,
    client,
    defaultMakeClientRequest,
    mkClientEnv,
    parseBaseUrl,
    runClientM,
  )
import Servant.Client.Core (addHeader, parseBaseUrl)
import System.Envy
  ( FromEnv (..),
    Option (customPrefix),
    defOption,
    gFromEnvCustom,
  )
import Text.Email.Parser (addrSpec, localPart)

data JiraConfig = JiraConfig
  { url :: Text,
    username :: Text,
    password :: Text,
    jql :: Text
  }
  deriving (Generic, Show)

instance FromEnv JiraConfig where
  fromEnv = gFromEnvCustom defOption {customPrefix = prefix "JIRA"}

data Person = Person
  { name :: Text,
    displayName :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON Person where
  parseJSON = withObject "person" $ \field -> Person <$> field .: "name" <*> field .: "displayName"

data ProjectCard = ProjectCard
  { key :: Text,
    projectName :: Text,
    people :: [Person]
  }
  deriving (Show, Eq, Generic)

instance FromJSON (Text -> ProjectCard) where
  parseJSON = withObject "card" $ \card -> do
    key <- card .: "key"
    fields <- card .: "fields"
    projectName <- fields .: "summary"
    allPossiblePeople <- M.fromList <$> mapM parser (HM.toList fields)
    pure $ \feild -> ProjectCard key projectName $ fromMaybe [] $ join $ lookup feild allPossiblePeople

parser :: FromJSON a => (Text, Value) -> Parser (Text, Maybe [a])
parser (x, field) = (x,) <$> (parseJSON field <|> fmap (replicate 1) <$> parseJSON field) <|> pure (x, Nothing)

newtype SearchResult = SearchResult {cards :: [ProjectCard]} deriving (Show, Eq, Generic)

instance FromJSON (Text -> SearchResult) where
  parseJSON = withObject "response" $ \response -> do
    cards <- sequence <$> response .: "issues"
    pure $ \field -> SearchResult $ cards field

data JiraField = JiraField
  { jiraFieldId :: Text,
    jiraFieldName :: Text
  }
  deriving (Generic, Show)

instance FromJSON JiraField where
  parseJSON = withObject "field" $ \field -> JiraField <$> field .: "id" <*> field .: "name"

type RequiredParam = QueryParam' '[Strict, Required]

type JiraAPI =
  "rest" :> "api" :> "latest" :> "search" :> RequiredParam "jql" Text :> RequiredParam "fields" Text :> RequiredParam "maxResults" Int :> Verb 'GET 200 '[JSON] (Text -> SearchResult)
    :<|> "rest" :> "api" :> "latest" :> "field" :> Verb 'GET 200 '[JSON] [JiraField]

searchForIssuesUsingJql :: Text -> Text -> Int -> ClientM (Text -> SearchResult)
obtainFieldConnfig :: ClientM [JiraField]
searchForIssuesUsingJql :<|> obtainFieldConnfig = client (Proxy :: Proxy JiraAPI)

runClient :: JiraConfig -> ClientM a -> IO a
runClient JiraConfig {..} cl = do
  manager <- liftIO $ newManager tlsManagerSettings
  serverUrl <- parseBaseUrl $ toString url
  result <- runClientM cl $ (mkClientEnv manager serverUrl) {makeClientRequest = const $ defaultMakeClientRequest serverUrl . addHeader (mk "Authorization") ("Basic " ++ decodeUtf8 (encode $ encodeUtf8 (username <> ":" <> password)))}
  return $ either (error . show) id result

projectCards :: Text -> JiraConfig -> IO [ProjectCard]
projectCards fieldName config@JiraConfig {..} = do
  runClient config $ do
    fieldsConfig <- obtainFieldConnfig
    let fieldsMap = M.fromList $ map (\(JiraField i n) -> (n, i)) fieldsConfig
        fieldId = fromMaybe (error "No field was found") $ lookup fieldName fieldsMap
    searchResults <- searchForIssuesUsingJql (replace "{fieldName}" fieldName jql) (fieldId <> ",summary") 1000
    pure $ cards $ searchResults fieldId
