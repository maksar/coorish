{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Jira (projectCards, JiraConfig, ProjectCard (key, projectName, people), Person (displayName)) where

import Data.Aeson
  ( FromJSON (parseJSON),
    decode,
    withObject,
    (.:),
    (.:?),
  )
import Data.Aeson.Types (Parser, Value (..))
import Data.ByteString.Base64 (encode)
import Data.CaseInsensitive (mk)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Data.Reflection
import Data.Text (replace)
import Env (prefix)
import GHC.TypeLits
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

instance Given String => FromJSON ProjectCard where
  parseJSON = withObject "card" $ \card -> do
    key <- card .: "key"
    fields <- card .: "fields"
    projectName <- fields .: "summary"
    peopleMaybe <- fields .:? fromString given <|> fmap (replicate 1) <$> fields .:? fromString given
    pure $ ProjectCard key projectName $ fromMaybe [] peopleMaybe

newtype SearchResult = SearchResult {cards :: [ProjectCard]} deriving (Show, Eq, Generic)

instance Given String => FromJSON SearchResult where
  parseJSON = withObject "response" $ \response -> SearchResult <$> response .: "issues"

data JiraField = JiraField
  { jiraFieldId :: Text,
    jiraFieldName :: Text
  }
  deriving (Generic, Show)

instance FromJSON JiraField where
  parseJSON = withObject "field" $ \field -> JiraField <$> field .: "id" <*> field .: "name"

type RequiredParam = QueryParam' '[Strict, Required]

type JiraAPI =
  "rest" :> "api" :> "latest" :> "search" :> RequiredParam "jql" Text :> RequiredParam "fields" Text :> RequiredParam "maxResults" Int :> Verb 'GET 200 '[JSON] SearchResult

type JiraInternalAPI = "rest" :> "api" :> "latest" :> "field" :> Verb 'GET 200 '[JSON] [JiraField]

obtainFieldId :: JiraConfig -> Text -> IO Text
obtainFieldId config fieldName = do
  runClient config $ do
    fieldsConfig <- client (Proxy :: Proxy JiraInternalAPI)
    let fieldsMap = M.fromList $ map (\(JiraField i n) -> (n, i)) fieldsConfig
    pure $ fromMaybe (error "No field was found") $ lookup fieldName fieldsMap

searchForIssuesUsingJql :: Given String => Text -> Text -> Int -> ClientM SearchResult
searchForIssuesUsingJql = client (Proxy :: Proxy JiraAPI)

runClient :: JiraConfig -> ClientM a -> IO a
runClient JiraConfig {..} cl = do
  manager <- liftIO $ newManager tlsManagerSettings
  serverUrl <- parseBaseUrl $ toString url
  result <- runClientM cl $ (mkClientEnv manager serverUrl) {makeClientRequest = const $ defaultMakeClientRequest serverUrl . addHeader (mk "Authorization") ("Basic " ++ decodeUtf8 (encode $ encodeUtf8 (username <> ":" <> password)))}
  return $ either (error . show) id result

projectCards :: Text -> JiraConfig -> IO [ProjectCard]
projectCards fieldName config@JiraConfig {..} = do
  fieldId <- obtainFieldId config fieldName
  runClient config $ cards <$> give (toString fieldId) searchForIssuesUsingJql (replace "{fieldName}" fieldName jql) (fieldId <> ",summary") 1000
