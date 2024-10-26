{-# LANGUAGE BlockArguments #-}

module Main (main) where

import Data.Maybe (Maybe (Nothing))
import Database.SQLite.Simple (withConnection)
import Myocardio.DatabaseNew (checkVersionNumber, migrateDatabase)
import System.IO (IO, print)

main :: IO ()
main = withConnection "/tmp/test.sqlite" \connection -> do
  migrateDatabase connection
