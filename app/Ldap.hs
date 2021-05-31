{-# LANGUAGE TemplateHaskell #-}

module Ldap (technicalCoordinators, LdapConfig) where

import Control.Monad.Extra (concatMapM)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.List (nub)
import Data.List.NonEmpty (fromList, toList)
import Data.Text (Text)
import qualified Data.Text as T
import Env (Config (ldapGroups), configValue, prefix)
import GHC.Generics (Generic)
import Ldap.Client
  ( Attr (Attr),
    Dn (..),
    Filter (And, Or, (:=)),
    Host (Tls),
    Ldap,
    Password (Password),
    PortNumber,
    Scope (WholeSubtree),
    SearchEntry (SearchEntry),
    bind,
    insecureTlsSettings,
    scope,
    search,
    with',
  )
import qualified Ldap.Client as L
import System.Envy
  ( FromEnv (..),
    Option (customPrefix),
    Var (..),
    defOption,
    gFromEnvCustom,
  )
import Text.LDAP.Data
  ( AttrType (AttrType),
    AttrValue (AttrValue),
    Component (S),
  )
import Text.LDAP.Parser (dn, runLdapParser)

data LdapConfig = LdapConfig
  { host :: Text,
    port :: PortNumber,
    username :: Text,
    password :: Text,
    groupsBase :: Text,
    usersBase :: Text,
    usersContainers :: [Text],
    groupsContainers :: [Text]
  }
  deriving (Generic, Show)

instance Var PortNumber where
  toVar = show
  fromVar = pure . read

instance FromEnv LdapConfig where
  fromEnv = gFromEnvCustom defOption {customPrefix = prefix "LDAP"}

withLdap :: LdapConfig -> (Ldap -> IO a) -> IO a
withLdap LdapConfig {..} rest = do
  with' (Tls (T.unpack host) insecureTlsSettings) port $ \ldap -> do
    bind ldap (Dn username) $ Password $ BSC.pack $ T.unpack password
    rest ldap

execute :: Ldap -> LdapConfig -> L.AttrValue -> Text -> [Filter] -> [Attr] -> IO [Dn]
execute ldap LdapConfig {..} objectClass base query attrs = do
  (SearchEntry _ al : _) <-
    search
      ldap
      (Dn base)
      (scope WholeSubtree)
      ( And $
          fromList
            [ Attr "objectClass" := objectClass,
              Or $ fromList query
            ]
      )
      attrs
  return $ extract attrs al
  where
    extract attrs groupAttrList = nub $ Prelude.concatMap (map (Dn . T.pack . BSC.unpack) . snd) $ filter (\attr -> fst attr `elem` attrs) groupAttrList

searchGroup :: Ldap -> LdapConfig -> Text -> IO [Dn]
searchGroup ldap config@LdapConfig {..} name = do
  members <-
    execute
      ldap
      config
      "group"
      groupsBase
      [ Attr "cn" := BSC.pack (T.unpack name),
        Attr "mailNickname" := BSC.pack (T.unpack name)
      ]
      [Attr "managedBy", Attr "msExchCoManagedByLink", Attr "member"]

  concatMapM extractGroup members
  where
    extractGroup group =
      if extractDn "OU" group `elem` groupsContainers
        then searchGroup ldap config (extractDn "CN" group)
        else pure [group]

searchUser :: Ldap -> LdapConfig -> Text -> IO [Dn]
searchUser ldap config@LdapConfig {..} dn =
  execute
    ldap
    config
    "user"
    usersBase
    [ Attr "cn" := BSC.pack (T.unpack dn)
    ]
    [Attr "displayName"]

extractDn :: BSC.ByteString -> Dn -> Text
extractDn part (Dn d) = T.pack $ BSC.unpack $ firstContainer part $ reverse $ toList $ either error id <$> runLdapParser dn $ BSLC.pack $ T.unpack d
  where
    firstContainer part ne = go ne
      where
        go (S (AttrType t, AttrValue v) : _) | t == part = v
        go (_ : xs) = go xs
        go [] = ""

technicalCoordinators :: LdapConfig -> IO [Text]
technicalCoordinators config@LdapConfig {..} = do
  withLdap config $ \ldap -> do
    groupMembers <- concatMapM (searchGroup ldap config) $(configValue ldapGroups)
    let members = map (extractDn "CN") $ filter (\dn -> extractDn "OU" dn `elem` usersContainers) groupMembers
    map (\(Dn d) -> d) <$> concatMapM (searchUser ldap config) members
