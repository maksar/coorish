{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad (forM_, unless, when)
import Data.List
import qualified Data.Text as T
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
      putStrLn $ T.unpack $ "Card '" <> Jira.projectName card <> "' (" <> Jira.key card <> ") has some people in '" <> fieldName <> "' field not from '" <> T.intercalate "; " $(configValue ldapGroups) <> "' AD group: '" <> T.intercalate "; " (map Jira.displayName invalidPeople) <> "'"

-- Jira.updateTechnicalCoordinators jiraConfig "PROJCARD-2243" validPeople
