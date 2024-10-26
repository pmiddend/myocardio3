{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Myocardio.DatabaseNew
  (
  )
where

import Control.Applicative (pure)
import Control.Monad (mapM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (forM_)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Maybe (Maybe (Just, Nothing), listToMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Database.SQLite.Simple (Connection, Only (Only, fromOnly), execute, execute_, query_, withTransaction)
import Myocardio.Database (Database)
import Myocardio.Database qualified as DatabaseJson
import Text.Show (show)
import Prelude (error)

newtype DatabaseUrl = DatabaseUrl Text

checkVersionNumber :: (MonadIO m) => Connection -> m (Maybe Int)
checkVersionNumber connection = do
  r <- liftIO $ query_ connection "SELECT version FROM Version LIMIT 1"
  pure (listToMaybe (fromOnly <$> r))

createDatabaseV1 :: (MonadIO m) => Connection -> m ()
createDatabaseV1 connection =
  liftIO $
    mapM_
      (execute_ connection)
      [ "CREATE TABLE IF NOT EXISTS Version ( version INTEGER NOT NULL )",
        "CREATE TABLE IF NOT EXISTS Exercise ( id INT PRIMARY KEY, name TEXT NOT NULL, category TEXT NOT NULL, description TEXT NOT NULL )",
        "CREATE TABLE IF NOT EXISTS ExerciseHasMuscle ( exercise_id INT NOT NULL, muscle TEXT NOT NULL )",
        "CREATE TABLE IF NOT EXISTS ExerciseHasFile ( exercise_id INT NOT NULL, file_path TEXT NOT NULL )",
        "CREATE TABLE IF NOT EXISTS ExerciseWithIntensity ( exercise_id INT NOT NULL, intensity INT NOT NULL, time INT NOT NULL, committed INT NOT NULL )",
        "CREATE TABLE IF NOT EXISTS Soreness ( muscle TEXT NOT NULL, soreness INT NOT NULL, time INT NOT NULL )"
      ]

migrateFromJson :: (MonadIO m) => Connection -> m ()
migrateFromJson connection = do
  dbFile <- DatabaseJson.getHomeDbFile
  oldDb <- DatabaseJson.readDatabase dbFile

  liftIO $ withTransaction connection do
    liftIO (execute connection "INSERT INTO Version (version) VALUES (?)" (Only (1 :: Int)))

migrateDatabase :: (MonadIO m) => Connection -> m ()
migrateDatabase connection = do
  createDatabaseV1 connection
  version <- checkVersionNumber connection
  case version of
    Nothing -> migrateFromJson connection
    Just 1 -> pure ()
    Just v -> error $ "invalid version stored in DB: expected 1, got " <> show v
