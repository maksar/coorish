{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Jira (projectCards, obtainFieldId, JiraConfig, ProjectCard (key, projectName, people), Person (displayName), Field (..)) where

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

data ProjectCard (n :: Symbol) = ProjectCard
  { key :: Text,
    projectName :: Text,
    people :: [Person]
  }
  deriving (Show, Eq, Generic)

instance KnownSymbol key => FromJSON (ProjectCard key) where
  parseJSON = withObject "card" $ \card -> do
    let name = symbolVal (Proxy @key)
    key <- card .: "key"
    fields <- card .: "fields"
    projectName <- fields .: "summary"
    peopleMaybe <- fields .:? fromString name <|> fmap (replicate 1) <$> fields .:? fromString name
    pure $ ProjectCard key projectName $ fromMaybe [] peopleMaybe

newtype SearchResult (key :: Symbol) = SearchResult {cards :: [ProjectCard key]} deriving (Show, Eq, Generic)

instance KnownSymbol key => FromJSON (SearchResult key) where
  parseJSON = withObject "response" $ \response -> SearchResult <$> response .: "issues"

data JiraField = JiraField
  { jiraFieldId :: Text,
    jiraFieldName :: Text
  }
  deriving (Generic, Show)

instance FromJSON JiraField where
  parseJSON = withObject "field" $ \field -> JiraField <$> field .: "id" <*> field .: "name"

type RequiredParam = QueryParam' '[Strict, Required]

type JiraAPI (key :: Symbol) =
  "rest" :> "api" :> "latest" :> "search" :> RequiredParam "jql" Text :> RequiredParam "fields" Text :> RequiredParam "maxResults" Int :> Verb 'GET 200 '[JSON] (SearchResult key)

type JiraInternalAPI = "rest" :> "api" :> "latest" :> "field" :> Verb 'GET 200 '[JSON] [JiraField]

obtainFieldId :: JiraConfig -> Text -> IO Text
obtainFieldId config fieldName = do
  runClient config $ do
    fieldsConfig <- client (Proxy :: Proxy JiraInternalAPI)
    let fieldsMap = M.fromList $ map (\(JiraField i n) -> (n, i)) fieldsConfig
    pure $ fromMaybe (error "No field was found") $ lookup fieldName fieldsMap

searchForIssuesUsingJql :: KnownSymbol n => Text -> Text -> Int -> ClientM (SearchResult n)
searchForIssuesUsingJql = client (Proxy :: Proxy (JiraAPI n))

runClient :: JiraConfig -> ClientM a -> IO a
runClient JiraConfig {..} cl = do
  manager <- liftIO $ newManager tlsManagerSettings
  serverUrl <- parseBaseUrl $ toString url
  result <- runClientM cl $ (mkClientEnv manager serverUrl) {makeClientRequest = const $ defaultMakeClientRequest serverUrl . addHeader (mk "Authorization") ("Basic " ++ decodeUtf8 (encode $ encodeUtf8 (username <> ":" <> password)))}
  return $ either (error . show) id result

data Field (key :: Symbol) = Field {fieldName :: Text, fieldId :: Text}

projectCards :: KnownSymbol key => Field key -> JiraConfig -> IO [ProjectCard key]
projectCards Field {..} config@JiraConfig {..} = do
  runClient config $ cards <$> searchForIssuesUsingJql (replace "{fieldName}" fieldName jql) (fieldId <> ",summary") 1000
