{-# LANGUAGE FlexibleContexts #-}

module Ldap (groupMembers, LdapConfig) where

import Env (Config, prefix)
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
import Relude
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
  fromVar = readMaybe

instance FromEnv LdapConfig where
  fromEnv = gFromEnvCustom defOption {customPrefix = prefix "LDAP"}

instance Ord Dn where
  compare (Dn a) (Dn b) = compare a b

withLdap :: LdapConfig -> (Ldap -> IO a) -> IO a
withLdap LdapConfig {..} rest = do
  with' (Tls (toString host) insecureTlsSettings) port $ \ldap -> do
    bind ldap (Dn username) $ Password $ encodeUtf8 password
    rest ldap

execute :: Ldap -> LdapConfig -> L.AttrValue -> Text -> NonEmpty Filter -> [Attr] -> IO [Dn]
execute ldap LdapConfig {..} objectClass base query attrs = do
  (SearchEntry _ al : _) <-
    search
      ldap
      (Dn base)
      (scope WholeSubtree)
      (And $ Attr "objectClass" := objectClass :| [Or query])
      attrs
  return $ extract attrs al
  where
    extract attrs groupAttrList = ordNub $ concatMap (map (Dn . decodeUtf8) . snd) $ filter (\attr -> fst attr `elem` attrs) groupAttrList

searchGroup :: Ldap -> LdapConfig -> Text -> IO [Dn]
searchGroup ldap config@LdapConfig {..} name = do
  members <-
    execute
      ldap
      config
      "group"
      groupsBase
      (Attr "cn" := encodeUtf8 name :| [Attr "mailNickname" := encodeUtf8 name])
      [Attr "managedBy", Attr "msExchCoManagedByLink", Attr "member"]

  join <$> mapM extractGroup members
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
    (Attr "cn" := encodeUtf8 dn :| [])
    [Attr "displayName"]

extractDn :: ByteString -> Dn -> Text
extractDn part (Dn d) = decodeUtf8 $ lastContainer part $ either (error . show) id <$> runLdapParser dn $ encodeUtf8 d
  where
    lastContainer part list = last $ mapMaybe (isS part) $ toList list

    last [] = error "Could not extract " <> part <> " from " <> encodeUtf8 d
    last [x] = x
    last (x : xs) = last xs

    isS part (S (AttrType t, AttrValue v)) | t == part = pure v
    isS _ _ = Nothing

groupMembers :: [Text] -> LdapConfig -> IO [Text]
groupMembers groups config@LdapConfig {..} = do
  withLdap config $ \ldap -> do
    groupMembers <- join <$> mapM (searchGroup ldap config) groups
    let members = map (extractDn "CN") $ filter (\dn -> extractDn "OU" dn `elem` usersContainers) groupMembers
    map (\(Dn d) -> d) . join <$> mapM (searchUser ldap config) members
