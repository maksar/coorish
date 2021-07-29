{-# LANGUAGE TypeApplications #-}

module Main where

import Coorish
import Env
import Relude

main :: IO ()
main = do
  config <- readConfig @Config
  forConfig config >>= mapM_ putTextLn
