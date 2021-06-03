{-# LANGUAGE TemplateHaskell #-}

module Main where

import Relude
import Control.Monad (forM_, unless, when)
import Data.List (partition)
import Env
  ( Config (jiraField, ldapGroups),
    configValue,
    readConfig,
  )
import qualified Jira
import qualified Ldap

main :: IO ()
main = do
  ldapConfig <- readConfig @Ldap.LdapConfig
  jiraConfig <- readConfig @Jira.JiraConfig
  fieldName <- Jira.fieldName jiraConfig $(configValue jiraField)

  activeDirectoryPeople <- Ldap.technicalCoordinators ldapConfig
  projectCards <- Jira.projectCards jiraConfig fieldName

  forM_ projectCards $ \card -> do
    let people = Jira.people card
    when (null people) $ pure ()

    let (validPeople, invalidPeople) = partition (\person -> Jira.displayName person `elem` activeDirectoryPeople) people

    unless (null invalidPeople) $ do
      putTextLn $ "Card '" <> Jira.projectName card <> "' (" <> Jira.key card <> ") has some people in '" <> fieldName <> "' field not from '" <> unwords (intersperse "; " $(configValue ldapGroups)) <> "' AD group: '" <> unwords (intersperse "; " (map Jira.displayName invalidPeople)) <> "'"

    -- Jira.updateTechnicalCoordinators jiraConfig (Jira.key card) validPeople
