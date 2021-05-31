{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Jira (projectCards, fieldName, updateTechnicalCoordinators, JiraConfig, ProjectCard (key, projectName, people), Person (displayName)) where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    ToJSON (toJSON),
    object,
    withObject,
    (.:),
    (.:?),
  )
import Data.Attoparsec.ByteString (parseOnly)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CI
import qualified Data.List.NonEmpty as NE
import Data.Map ((!))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Proxy as P
import Data.Text (Text)
import qualified Data.Text as T
import Env (Config (jiraField), configValue, prefix)
import GHC.Generics (Generic)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
  ( Capture,
    JSON,
    NoContent,
    QueryParam',
    ReqBody,
    Required,
    StdMethod (GET, PUT),
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

instance FromJSON ProjectCard where
  parseJSON = withObject "card" $ \card -> do
    key <- card .: "key"
    fields <- card .: "fields"
    projectName <- fields .: "summary"
    peopleMaybe <- fields .:? $(configValue jiraField) <|> fmap (replicate 1) <$> fields .:? $(configValue jiraField)
    pure $ ProjectCard key projectName $ fromMaybe [] peopleMaybe

newtype SearchResult = SearchResult {cards :: [ProjectCard]} deriving (Show, Eq, Generic)

instance FromJSON SearchResult where
  parseJSON = withObject "response" $ \response -> SearchResult <$> response .: "issues"

newtype Coordinator = Coordinator {_name :: Text} deriving (Show, Eq)

instance ToJSON Coordinator where
  toJSON Coordinator {..} = object ["name" .= _name]

fromPerson :: Person -> Coordinator
fromPerson Person {..} = Coordinator $ T.pack $ BS.unpack $ localPart $ either error id $ parseOnly addrSpec (BS.pack $ T.unpack name)

newtype UpdatePayload = UpdatePayload {coordinators :: [Coordinator]} deriving (Show, Eq)

instance ToJSON UpdatePayload where
  toJSON UpdatePayload {..} = do
    object ["fields" .= object [$(configValue jiraField) .= coordinators]]

data JiraField = JiraField
  { jiraFieldId :: Text,
    jiraFieldName :: Text
  }
  deriving (Generic, Show)

instance FromJSON JiraField where
  parseJSON = withObject "field" $ \field -> do
    id <- field .: "id"
    name <- field .: "name"
    pure $ JiraField id name

type RequiredParam = QueryParam' '[Strict, Required]

type JiraAPI =
  "rest" :> "api" :> "latest" :> "search" :> RequiredParam "jql" Text :> RequiredParam "fields" Text :> RequiredParam "maxResults" Int :> Verb 'GET 200 '[JSON] SearchResult
    :<|> "rest" :> "api" :> "latest" :> "issue" :> Capture "key" Text :> ReqBody '[JSON] UpdatePayload :> Verb 'PUT 200 '[JSON] NoContent
    :<|> "rest" :> "api" :> "latest" :> "field" :> Verb 'GET 200 '[JSON] [JiraField]

searchForIssuesUsingJql :: Text -> Text -> Int -> ClientM SearchResult
updateTechnicalCoordinators_ :: Text -> UpdatePayload -> ClientM NoContent
obtainFieldConnfig :: ClientM [JiraField]
searchForIssuesUsingJql :<|> updateTechnicalCoordinators_ :<|> obtainFieldConnfig = client (P.Proxy :: P.Proxy JiraAPI)

updateTechnicalCoordinators :: JiraConfig -> Text -> [Person] -> IO ()
updateTechnicalCoordinators config projectKey people = do
  runClient config $ do
    updateTechnicalCoordinators_ projectKey $ UpdatePayload $ map fromPerson people
  pure ()

runClient :: JiraConfig -> ClientM a -> IO a
runClient JiraConfig {..} cl = do
  manager <- liftIO $ newManager tlsManagerSettings
  url <- parseBaseUrl $ T.unpack url
  result <- runClientM cl $ (mkClientEnv manager url) {makeClientRequest = const $ defaultMakeClientRequest url . addHeader (CI.mk "Authorization") ("Basic " ++ BS.unpack (Base64.encode $ BS.pack $ T.unpack (username <> ":" <> password)))}
  return $ either (error . show) id result

projectCards :: JiraConfig -> Text -> IO [ProjectCard]
projectCards config@JiraConfig {..} fieldName = do
  result <- runClient config $ do
    searchForIssuesUsingJql (T.replace "{fieldName}" fieldName jql) ($(configValue jiraField) <> ",summary") 1000
  pure $ cards result

fieldName :: JiraConfig -> Text -> IO Text
fieldName config name = do
  result <- runClient config obtainFieldConnfig
  return $ M.fromList (Prelude.map toPair result) ! $(configValue jiraField)
  where
    toPair f = (jiraFieldId f, jiraFieldName f)
