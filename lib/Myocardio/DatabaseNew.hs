{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Myocardio.DatabaseNew
  ( withDatabase,
    retrieveExercisesWithCurrentIntensity,
    retrieveExercisesDescriptions,
    ExerciseWithIntensity (..),
    retrieveFile,
    ExerciseDescription (..),
    retrieveAllMuscles,
    Muscle (..),
    Soreness (..),
    ExerciseCommitted (..),
    retrieveSoreness,
  )
where

import Control.Applicative (pure)
import Control.Exception (catch)
import Control.Monad (mapM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Bool (Bool (True))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (forM_, for_)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Int (Int)
import Data.List (head)
import Data.Map.Strict qualified as Map
import Data.Maybe (Maybe (Just, Nothing), fromMaybe, listToMaybe)
import Data.Monoid (mempty)
import Data.Ord (Ord)
import Data.Semigroup ((<>))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (String)
import Data.Text (Text, pack, unpack)
import Data.Tuple (uncurry)
import Database.SQLite.Simple (Connection, Only (Only, fromOnly), Query, SQLError, ToRow, close, execute, execute_, lastInsertRowId, open, query, query_, withTransaction)
import Myocardio.Database qualified as DatabaseJson
import Myocardio.Database qualified as OldDb
import System.Directory (createDirectoryIfMissing)
import System.Environment.XDG.BaseDir (getUserConfigDir, getUserDataDir)
import System.IO (FilePath, IO)
import Text.Show (Show, show)
import UnliftIO.Exception (bracket)
import Prelude (error)

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
        "CREATE TABLE IF NOT EXISTS ExerciseHasMuscle ( exercise_id INTEGER NOT NULL, muscle_id INTEGER NOT NULL, FOREIGN KEY (exercise_id) REFERENCES Exercise(id), FOREIGN KEY (muscle_id) REFERENCES Muscle(id) )",
        "CREATE TABLE IF NOT EXISTS ExerciseHasFile ( id INTEGER PRIMARY KEY, exercise_id INTEGER NOT NULL, file_content BLOB NOT NULL, FOREIGN KEY (exercise_id) REFERENCES Exercise(id) )",
        "CREATE TABLE IF NOT EXISTS ExerciseWithIntensity ( exercise_id INTEGER NOT NULL, intensity TEXT NOT NULL, time INTEGER NOT NULL, committed INTEGER NOT NULL, FOREIGN KEY (exercise_id) REFERENCES Exercise(id) )",
        "CREATE TABLE IF NOT EXISTS Soreness ( muscle_id INTEGER NOT NULL, soreness INTEGER NOT NULL, time INTEGER NOT NULL, FOREIGN KEY (muscle_id) REFERENCES Muscle(id) )"
      ]

getUploadedFileDir :: (MonadIO m) => m FilePath
getUploadedFileDir = do
  uploadedFilesBaseDir <- liftIO $ getUserDataDir "myocardio3"
  pure (uploadedFilesBaseDir <> "/uploaded-files")

insertMMap :: (Ord k) => IORef (Map.Map k a) -> k -> a -> IO ()
insertMMap ref key value = modifyIORef ref (Map.insert key value)

insertMSet :: (Ord a) => IORef (Set.Set a) -> a -> IO ()
insertMSet ref value = modifyIORef ref (Set.insert value)

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
        ( OldDb.getName exercise.name,
          exercise.description
        )
      exerciseId <- lastInsertRowId connection
      insertMMap exerciseNameToId exercise.name exerciseId
      forM_ exercise.muscles \muscle -> do
        nameToId <- readIORef muscleNameToId
        muscleId <- case Map.lookup (show muscle) nameToId of
          Nothing -> do
            run "INSERT INTO Muscle (name) VALUES (?)" (Only (pack (show muscle)))
            lastInsertRowId connection
          Just muscleId' -> pure muscleId'

        insertMMap muscleNameToId (show muscle) muscleId

        run
          "INSERT INTO ExerciseHasMuscle (exercise_id, muscle_id) VALUES (?, ?)"
          (exerciseId, muscleId)
      forM_ exercise.fileReferences \fileRef -> do
        uploadedFileDir <- getUploadedFileDir
        contents <- BS.readFile (uploadedFileDir <> "/" <> unpack (OldDb.getFileReference fileRef))
        run
          "INSERT INTO ExerciseHasFile (exercise_id, file_content) VALUES (?, ?)"
          (exerciseId, contents)

    forM_ oldDb.sorenessHistory \soreness -> do
      nameToId <- readIORef muscleNameToId
      case Map.lookup (show soreness.muscle) nameToId of
        Nothing -> error $ "couldn't find muscle " <> show soreness.muscle
        Just muscleId -> do
          let sorenessToInt :: OldDb.SorenessValue -> Int
              sorenessToInt OldDb.VerySore = 2
              sorenessToInt OldDb.LittleSore = 1
              sorenessToInt OldDb.NotSore = 0
          run
            "INSERT INTO Soreness (muscle_id, soreness, time) VALUES (?, ?, ?)"
            (muscleId, sorenessToInt soreness.soreness, soreness.time)

    forM_ oldDb.pastExercises \exerciseWithIntensity -> do
      nameToId <- readIORef exerciseNameToId
      case Map.lookup exerciseWithIntensity.exercise.name nameToId of
        Nothing -> error $ "couldn't find exercise " <> show exerciseWithIntensity.exercise.name
        Just exerciseId -> do
          run "INSERT INTO ExerciseWithIntensity (exercise_id, intensity, time, committed) VALUES (?, ?, ?, ?)" (exerciseId, OldDb.getIntensity exerciseWithIntensity.intensity, exerciseWithIntensity.time, 1 :: Int)

migrateDatabase :: (MonadIO m) => Connection -> m ()
migrateDatabase connection = do
  version <- checkVersionNumber connection
  createDatabaseV1 connection
  case version of
    Nothing -> migrateFromJson connection
    Just 1 -> pure ()
    Just v -> error $ "invalid version stored in DB: expected 1, got " <> show v

-- | sqlite-simple does provide withConnection, but it's only in IO, and because of scotty, we need it in a more arbitrary monad
withConnection :: (MonadUnliftIO m) => String -> (Connection -> m a) -> m a
withConnection url = bracket (liftIO (open url)) (liftIO . close)

-- | Open a connect, migrate the DB if needed, and give the connection to the user
withDatabase :: (MonadUnliftIO m) => (Connection -> m b) -> m b
withDatabase f = do
  configBaseDir <- liftIO $ getUserConfigDir "myocardio3"
  liftIO $ createDirectoryIfMissing True configBaseDir
  withConnection (configBaseDir <> "/" <> "db.sqlite") \connection -> do
    liftIO (execute_ connection "PRAGMA foreign_keys = ON;")
    migrateDatabase connection
    f connection

data ExerciseWithIntensity = ExerciseWithIntensity
  { id :: Int,
    muscles :: !(Set.Set Text),
    description :: !Text,
    name :: !Text,
    fileIds :: !(Set Int),
    intensity :: Text
  }
  deriving (Show)

multiInsert :: (Ord k, Ord p) => k -> p -> Map.Map k (Set.Set p) -> Map.Map k (Set.Set p)
multiInsert key newValue = Map.alter alterer key
  where
    alterer Nothing = Just (Set.singleton newValue)
    alterer (Just oldSet) = Just (Set.insert newValue oldSet)

multiInsertMMap :: (Ord k, Ord p) => IORef (Map.Map k (Set.Set p)) -> k -> p -> IO ()
multiInsertMMap ref key newValue = modifyIORef ref (multiInsert key newValue)

data Soreness = Soreness
  { muscle :: !Text,
    soreness :: !Int
  }
  deriving (Show)

-- For every muscle, calculate the latest soreness value
retrieveSoreness :: Connection -> IO [Soreness]
retrieveSoreness connection = do
  -- Courtesy of
  -- https://stackoverflow.com/questions/17327043/how-can-i-select-rows-with-most-recent-timestamp-for-each-key-value
  results <-
    query_
      connection
      "SELECT \
      \ M.name, L.muscle_id, L.soreness \
      \ FROM Soreness L \
      \ LEFT JOIN Soreness R ON L.muscle_id = R.muscle_id AND L.time < R.time \
      \ INNER JOIN Muscle M ON M.id == L.muscle_id \
      \ WHERE R.muscle_id IS NULL AND L.soreness > 0" ::
      IO [(Text, Int, Int)]

  pure ((\(muscleName, _muscleId, soreness) -> Soreness muscleName soreness) <$> results)

data ExerciseCommitted = Committed | NotCommitted

retrieveExercisesWithCurrentIntensity :: Connection -> ExerciseCommitted -> IO [ExerciseWithIntensity]
retrieveExercisesWithCurrentIntensity conn committed = do
  results <-
    query
      conn
      "SELECT E.id, E.description, E.name, M.name muscle_name, F.id file_id, WI.intensity \
      \  FROM Exercise as E \
      \  LEFT JOIN ExerciseHasMuscle ON E.id = ExerciseHasMuscle.exercise_id \
      \  LEFT JOIN Muscle M ON M.id = ExerciseHasMuscle.muscle_id \
      \  LEFT JOIN ExerciseHasFile F ON E.id = F.exercise_id \
      \  LEFT JOIN ExerciseWithIntensity WI ON E.id = WI.exercise_id \
      \  WHERE WI.committed = ?"
      (Only @Int (case committed of Committed -> 1; NotCommitted -> 0)) ::
      IO [(Int, Text, Text, Maybe Text, Maybe Int, Maybe Text)]

  exerciseDataRef <- newIORef mempty
  exerciseMusclesByIdRef <- newIORef mempty
  exerciseFilesByIdRef <- newIORef mempty
  exerciseToIntensityRef <- newIORef mempty
  forM_ results \(exId, exDescription, exName, muName, fileId, intensity) -> do
    insertMSet exerciseDataRef (exId, exDescription, exName)

    for_ muName (multiInsertMMap exerciseMusclesByIdRef exId)
    for_ fileId (multiInsertMMap exerciseFilesByIdRef exId)

    for_ intensity (insertMMap exerciseToIntensityRef exId)

  exerciseData <- readIORef exerciseDataRef
  exerciseMusclesById <- readIORef exerciseMusclesByIdRef
  exerciseFilesById <- readIORef exerciseFilesByIdRef
  exerciseToIntensity <- readIORef exerciseToIntensityRef

  pure
    ( ( \(exId, exDescription, exName) ->
          ExerciseWithIntensity
            { id = exId,
              description = exDescription,
              name = exName,
              muscles = fromMaybe mempty (Map.lookup exId exerciseMusclesById),
              fileIds = fromMaybe mempty (Map.lookup exId exerciseFilesById),
              intensity = fromMaybe "N/A" (Map.lookup exId exerciseToIntensity)
            }
      )
        <$> Set.toList exerciseData
    )

data Muscle = Muscle {id :: Int, name :: Text} deriving (Show)

retrieveAllMuscles :: Connection -> IO [Muscle]
retrieveAllMuscles connection = do
  results <- query_ connection "SELECT id, name FROM Muscle ORDER BY name ASC" :: IO [(Int, Text)]
  pure (uncurry Muscle <$> results)

data ExerciseDescription = ExerciseDescription
  { id :: Int,
    muscles :: !(Set.Set Text),
    description :: !Text,
    name :: !Text,
    fileIds :: !(Set Int)
  }
  deriving (Show)

retrieveExercisesDescriptions :: Connection -> IO [ExerciseDescription]
retrieveExercisesDescriptions conn = do
  results <-
    query_
      conn
      "SELECT E.id, E.description, E.name, M.name muscle_name, F.id file_id \
      \  FROM Exercise as E \
      \  LEFT JOIN ExerciseHasMuscle ON E.id = ExerciseHasMuscle.exercise_id \
      \  LEFT JOIN Muscle M ON M.id = ExerciseHasMuscle.muscle_id \
      \  LEFT JOIN ExerciseHasFile F ON E.id = F.exercise_id \
      \  ORDER BY E.name ASC" ::
      IO [(Int, Text, Text, Maybe Text, Maybe Int)]

  exerciseDataRef <- newIORef mempty
  exerciseMusclesByIdRef <- newIORef mempty
  exerciseFilesByIdRef <- newIORef mempty
  forM_ results \(exId, exDescription, exName, muName, fileId) -> do
    insertMSet exerciseDataRef (exId, exDescription, exName)

    for_ muName (multiInsertMMap exerciseMusclesByIdRef exId)
    for_ fileId (multiInsertMMap exerciseFilesByIdRef exId)

  exerciseData <- readIORef exerciseDataRef
  exerciseMusclesById <- readIORef exerciseMusclesByIdRef
  exerciseFilesById <- readIORef exerciseFilesByIdRef

  pure
    ( ( \(exId, exDescription, exName) ->
          ExerciseDescription
            { id = exId,
              description = exDescription,
              name = exName,
              muscles = fromMaybe mempty (Map.lookup exId exerciseMusclesById),
              fileIds = fromMaybe mempty (Map.lookup exId exerciseFilesById)
            }
      )
        <$> Set.toList exerciseData
    )

retrieveFile :: Connection -> Int -> IO BSL.ByteString
retrieveFile conn fileId = (fromOnly . head) <$> (query conn "SELECT file_content FROM ExerciseHasFile WHERE id = ?" (Only fileId))
