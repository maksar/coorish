{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Coorish (forConfig) where

import Control.Monad
import Data.List (partition)
import Data.Reflection
import Env
import GHC.TypeLits
import qualified Jira
import qualified Ldap
import Relude

whenNotEmpty :: [a] -> IO (Maybe b) -> IO (Maybe b)
whenNotEmpty list next = if null list then pure Nothing else next

forConfig :: Config -> IO [Text]
forConfig Config {..} = do
  ldapConfig <- readConfig @Ldap.LdapConfig
  jiraConfig <- readConfig @Jira.JiraConfig

  activeDirectoryPeople <- Ldap.groupMembers ldapGroups ldapConfig

  projectCards <- Jira.projectCards jiraField jiraConfig

  flip mapMaybeM projectCards $ \card -> do
    let people = Jira.people card
    whenNotEmpty people $ do
      let (_validPeople, invalidPeople) = partition (\person -> Jira.displayName person `elem` activeDirectoryPeople) people
      whenNotEmpty invalidPeople $ do
        return $
          Just $
            "Card '" <> Jira.projectName card <> "' (" <> Jira.key card <> ") "
              <> "has some people in '"
              <> jiraField
              <> "' field not from '"
              <> mconcat (intersperse "; " ldapGroups)
              <> "' AD group: '"
              <> mconcat (intersperse "; " (map Jira.displayName invalidPeople))
              <> "'"
