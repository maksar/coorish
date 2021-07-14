{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad
import Data.List (partition)
import Env
import qualified Jira
import qualified Ldap
import Relude

main :: IO ()
main = do
  Config {..} <- readConfig @Config
  ldapConfig <- readConfig @Ldap.LdapConfig
  jiraConfig <- readConfig @Jira.JiraConfig

  activeDirectoryPeople <- Ldap.technicalCoordinators ldapGroups ldapConfig
  projectCards <- Jira.projectCards jiraField jiraConfig

  forM_ projectCards $ \card -> do
    let people = Jira.people card
    when (null people) $ pure ()

    let (validPeople, invalidPeople) = partition (\person -> Jira.displayName person `elem` activeDirectoryPeople) people

    unless (null invalidPeople) $ do
      putTextLn $ "Card '" <> Jira.projectName card <> "' (" <> Jira.key card <> ") has some people in '" <> jiraField <> "' field not from '" <> mconcat (intersperse "; " ldapGroups) <> "' AD group: '" <> mconcat (intersperse "; " (map Jira.displayName invalidPeople)) <> "'"
