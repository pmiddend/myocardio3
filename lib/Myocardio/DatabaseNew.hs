{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Myocardio.DatabaseNew
  ( checkVersionNumber,
    migrateDatabase,
  )
where

import Control.Applicative (pure)
import Control.Exception (catch)
import Control.Monad (mapM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (forM_)
import Data.ByteString qualified as BS
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Maybe (Maybe (Just, Nothing), listToMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text, pack, unpack)
import Database.SQLite.Simple (Connection, Only (Only, fromOnly), Query, SQLError, ToRow, execute, execute_, lastInsertRowId, query_, withTransaction)
import Myocardio.Database (getName, fileReferences, getFileReference, SorenessValue (VerySore, LittleSore, NotSore), getIntensity)
import Myocardio.Database qualified as DatabaseJson
import System.IO (IO)
import Text.Show (show)
import Prelude (error)
import Data.IORef (newIORef, modifyIORef, readIORef)
import Data.Monoid (mempty)
import qualified Data.Map.Strict as Map
import System.IO (FilePath)
import System.Environment.XDG.BaseDir (getUserDataDir)

newtype DatabaseUrl = DatabaseUrl Text

checkVersionNumber :: (MonadIO m) => Connection -> m (Maybe Int)
checkVersionNumber connection =
  let action = do
        -- This might fail, because we don't even have a version table
        -- yet! See catch below, because here it's fine to just treat
        -- the error as a benign "not found".
        r <- query_ connection "SELECT version FROM Version LIMIT 1"
        pure (listToMaybe (fromOnly <$> r))
   in liftIO (action `catch` (\(_ :: SQLError) -> pure Nothing))

createDatabaseV1 :: (MonadIO m) => Connection -> m ()
createDatabaseV1 connection =
  liftIO $
    mapM_
      (execute_ connection)
      [ "CREATE TABLE IF NOT EXISTS Version ( version INTEGER NOT NULL )",
        "CREATE TABLE IF NOT EXISTS Exercise ( id INTEGER PRIMARY KEY, name TEXT NOT NULL, description TEXT NOT NULL )",
        "CREATE TABLE IF NOT EXISTS Muscle ( id INTEGER PRIMARY KEY, name TEXT NOT NULL )",
        "CREATE TABLE IF NOT EXISTS ExerciseHasMuscle ( exercise_id INTEGER NOT NULL, muscle_id INTEGER NOT NULL )",
        "CREATE TABLE IF NOT EXISTS ExerciseHasFile ( exercise_id INTEGER NOT NULL, file_content BLOB NOT NULL )",
        "CREATE TABLE IF NOT EXISTS ExerciseWithIntensity ( exercise_id INTEGER NOT NULL, intensity TEXT NOT NULL, time INTEGER NOT NULL, committed INTEGER NOT NULL )",
        "CREATE TABLE IF NOT EXISTS Soreness ( muscle_id INTEGER NOT NULL, soreness INTEGER NOT NULL, time INTEGER NOT NULL )"
      ]
      
getUploadedFileDir :: (MonadIO m) => m FilePath
getUploadedFileDir = do
  uploadedFilesBaseDir <- liftIO $ getUserDataDir "myocardio3"
  pure (uploadedFilesBaseDir <> "/uploaded-files")

migrateFromJson :: (MonadIO m) => Connection -> m ()
migrateFromJson connection = do
  dbFile <- DatabaseJson.getHomeDbFile
  oldDb <- DatabaseJson.readDatabase dbFile

  liftIO $ withTransaction connection do
    let run :: (ToRow q) => Query -> q -> IO ()
        run = execute connection
    run "INSERT INTO Version (version) VALUES (?)" (Only (1 :: Int))
    exerciseNameToId <- newIORef mempty
    muscleNameToId <- newIORef mempty
    forM_ oldDb.exercises \exercise -> do
      run
        "INSERT INTO Exercise (name, description) VALUES (?, ?)"
        ( getName exercise.name,
          exercise.description
        )
      exerciseId <- lastInsertRowId connection
      modifyIORef exerciseNameToId (Map.insert exercise.name exerciseId)
      forM_ exercise.muscles \muscle -> do
        nameToId <- readIORef muscleNameToId
        muscleId <- case Map.lookup (show muscle) nameToId of
          Nothing -> do
            run "INSERT INTO Muscle (name) VALUES (?)" (Only (pack (show muscle)))
            lastInsertRowId connection
          Just muscleId' -> pure muscleId'
        modifyIORef muscleNameToId (Map.insert (show muscle) muscleId)
        run
          "INSERT INTO ExerciseHasMuscle (exercise_id, muscle_id) VALUES (?, ?)"
          (exerciseId, muscleId)
      forM_ exercise.fileReferences \fileRef -> do
        uploadedFileDir <- getUploadedFileDir
        contents <- BS.readFile (uploadedFileDir <> "/" <> unpack (getFileReference fileRef))
        run
          "INSERT INTO ExerciseHasFile (exercise_id, file_content) VALUES (?, ?)"
          (exerciseId, contents)

    forM_ oldDb.sorenessHistory \soreness -> do
      nameToId <- readIORef muscleNameToId
      case Map.lookup (show soreness.muscle) nameToId of
        Nothing -> error $ "couldn't find muscle " <> show soreness.muscle
        Just muscleId -> do
          let sorenessToInt :: SorenessValue -> Int
              sorenessToInt VerySore = 2
              sorenessToInt LittleSore = 1
              sorenessToInt NotSore = 0
          run
            "INSERT INTO Soreness (muscle_id, soreness, time) VALUES (?, ?, ?)"
            (muscleId, sorenessToInt soreness.soreness, soreness.time)

    forM_ oldDb.pastExercises \exerciseWithIntensity -> do
      nameToId <- readIORef exerciseNameToId
      case Map.lookup exerciseWithIntensity.exercise.name nameToId of
        Nothing -> error $ "couldn't find exercise " <> show exerciseWithIntensity.exercise.name
        Just exerciseId -> do
          run "INSERT INTO ExerciseWithIntensity (exercise_id, intensity, time, committed) VALUES (?, ?, ?, ?)" (exerciseId, getIntensity exerciseWithIntensity.intensity, exerciseWithIntensity.time, 1 :: Int)
        

      

migrateDatabase :: (MonadIO m) => Connection -> m ()
migrateDatabase connection = do
  version <- checkVersionNumber connection
  createDatabaseV1 connection
  case version of
    Nothing -> migrateFromJson connection
    Just 1 -> pure ()
    Just v -> error $ "invalid version stored in DB: expected 1, got " <> show v
