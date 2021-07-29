module Env (readConfig, prefix, Config (..)) where

import Data.Char (toUpper)
import Data.Text (splitOn)
import Relude
import System.Envy

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
