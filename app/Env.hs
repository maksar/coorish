{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Env (readConfig, prefix, Config (jiraField, ldapGroups), configValue) where

import Relude
import Data.Text (splitOn)
import Data.Char (toUpper)
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Exp, Lift, Q, runIO)
import System.Envy
  ( FromEnv (..),
    Option (customPrefix, dropPrefixCount),
    Var (..),
    decodeEnv,
    defOption,
    gFromEnvCustom,
  )

data Config = Config
  { jiraField :: Text,
    ldapGroups :: [Text]
  }
  deriving (Generic, Show)

instance Var [Text] where
  toVar = show
  fromVar = pure . splitOn "," . toText

base :: String
base = "COORISH"

prefix :: String -> String
prefix name = intercalate "_" [base, map toUpper name]

instance FromEnv Config where
  fromEnv = gFromEnvCustom defOption {customPrefix = base}

readConfig :: FromEnv a => IO a
readConfig = either (error . show) id <$> decodeEnv

configValue :: Lift t => (Config -> t) -> Q Exp
configValue f = do
  groups <- runIO (f <$> readConfig @Config)
  [e|groups|]
