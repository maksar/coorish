{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Env (readConfig, prefix, Config (jiraField, ldapGroups), configValue) where

import Data.Char (toUpper)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
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
  fromVar = pure . T.splitOn "," . T.pack

base :: String
base = "COORISH"

prefix :: String -> String
prefix name = intercalate "_" [base, map toUpper name]

instance FromEnv Config where
  fromEnv = gFromEnvCustom defOption {customPrefix = base}

readConfig :: FromEnv a => IO a
readConfig = either error id <$> decodeEnv

configValue :: Lift t => (Config -> t) -> Q Exp
configValue f = do
  groups <- runIO (f <$> readConfig @Config)
  [e|groups|]
