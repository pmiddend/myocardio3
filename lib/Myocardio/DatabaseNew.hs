{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Myocardio.DatabaseNew
  ( ExerciseWithWorkouts (..),
    ExerciseDescription (..),
    ExerciseWorkout (..),
    Muscle (..),
    Soreness (..),
    ExerciseCommitted (..),
    MuscleWithWorkout (..),
    MigrationFlags (..),
    IdType,
    withDatabase,
    notSore,
    littleSore,
    verySore,
    retrieveFile,
    migrateDatabase,
    openDatabase,
    closeDatabase,
    retrieveAllMuscles,
    retrieveExercisesWithWorkouts,
    retrieveExercisesDescriptions,
    retrieveCurrentSoreness,
    retrieveSorenessHistory,
    retrieveMusclesWithLastWorkoutTime,
    insertMuscle,
    insertExercise,
    updateExercise,
    removeExercise,
    uploadFileForExercise,
    toggleExercise,
    changeIntensity,
    updateSoreness,
    commitWorkout,
    Connection,
  )
where

import Control.Applicative (pure, (<*>))
import Control.Exception (catch)
import Control.Monad (mapM_, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Bool (Bool (True))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Eq (Eq ((==)))
import Data.Foldable (forM_, for_)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.IORef (newIORef, readIORef)
import Data.Int (Int, Int64)
import Data.List (head)
import Data.Map.Strict qualified as Map
import Data.Maybe (Maybe (Just, Nothing), fromMaybe, listToMaybe)
import Data.Monoid (mempty)
import Data.Ord (Ord ((<=)), (>))
import Data.Semigroup ((<>))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (String)
import Data.Text (Text, pack, unpack)
import Data.Time (UTCTime)
import Data.Tuple (uncurry)
import Database.SQLite.Simple (Connection, Only (Only, fromOnly), Query, SQLError, ToRow, changes, close, execute, execute_, lastInsertRowId, open, query, query_, withTransaction)
import Myocardio.DatabaseOld qualified as DatabaseJson
import Myocardio.DatabaseOld qualified as OldDb
import Myocardio.MapUtils (insertMMap, insertMSet, multiInsertMMap)
import System.Directory (createDirectoryIfMissing)
import System.Environment.XDG.BaseDir (getUserConfigDir, getUserDataDir)
import System.IO (FilePath, IO)
import Text.Show (Show, show)
import UnliftIO.Exception (bracket)
import Prelude (error)

type IdType = Int64

data Muscle = Muscle {id :: !IdType, name :: !Text} deriving (Show)

instance Eq Muscle where
  m1 == m2 = m1.id == m2.id

instance Ord Muscle where
  m1 <= m2 = m1.id <= m2.id

data ExerciseWorkout = ExerciseWorkout
  { exerciseId :: !IdType,
    time :: !UTCTime,
    committed :: !Bool,
    intensity :: !Text
  }
  -- We need Ord because we want to store it in a multi map
  deriving (Show, Eq, Ord)

data ExerciseWithWorkouts = ExerciseWithWorkouts
  { id :: !IdType,
    muscles :: !(Set.Set Muscle),
    description :: !Text,
    name :: !Text,
    fileIds :: !(Set IdType),
    workouts :: !(Set ExerciseWorkout)
  }
  deriving (Show)

data MuscleWithWorkout = MuscleWithWorkout
  { muscleId :: !IdType,
    muscleName :: !Text,
    workoutTime :: !UTCTime
  }

data Soreness = Soreness
  { muscleId :: !IdType,
    muscleName :: !Text,
    soreness :: !Int,
    time :: !UTCTime
  }
  deriving (Show)

notSore :: Int
notSore = 0

littleSore :: Int
littleSore = 1

verySore :: Int
verySore = 2

data ExerciseDescription = ExerciseDescription
  { id :: !IdType,
    muscles :: !(Set.Set Muscle),
    description :: !Text,
    name :: !Text,
    fileIds :: !(Set IdType)
  }
  deriving (Show)

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
        "CREATE TABLE IF NOT EXISTS ExerciseHasMuscle ( exercise_id INTEGER NOT NULL, muscle_id INTEGER NOT NULL, FOREIGN KEY (exercise_id) REFERENCES Exercise(id) ON DELETE CASCADE, FOREIGN KEY (muscle_id) REFERENCES Muscle(id) ON DELETE CASCADE )",
        "CREATE TABLE IF NOT EXISTS ExerciseHasFile ( id INTEGER PRIMARY KEY, exercise_id INTEGER NOT NULL, file_content BLOB NOT NULL, FOREIGN KEY (exercise_id) REFERENCES Exercise(id) ON DELETE CASCADE )",
        "CREATE TABLE IF NOT EXISTS ExerciseWithIntensity ( exercise_id INTEGER NOT NULL, intensity TEXT NOT NULL, time INTEGER NOT NULL, committed INTEGER NOT NULL, FOREIGN KEY (exercise_id) REFERENCES Exercise(id) ON DELETE CASCADE )",
        "CREATE TABLE IF NOT EXISTS Soreness ( muscle_id INTEGER NOT NULL, soreness INTEGER NOT NULL, time INTEGER NOT NULL, FOREIGN KEY (muscle_id) REFERENCES Muscle(id) ON DELETE CASCADE )"
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

data MigrationFlags = MigrateFromJson | NoJson

migrateDatabase :: (MonadIO m) => Connection -> MigrationFlags -> m ()
migrateDatabase connection flags = do
  version <- checkVersionNumber connection
  createDatabaseV1 connection
  case version of
    Nothing ->
      case flags of
        MigrateFromJson -> migrateFromJson connection
        NoJson -> pure ()
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
    migrateDatabase connection MigrateFromJson
    f connection

openDatabase :: (MonadIO m) => String -> m Connection
openDatabase url = liftIO (open url)

closeDatabase :: (MonadIO m) => Connection -> m ()
closeDatabase = liftIO . close

-- For every muscle, calculate the latest soreness value
retrieveCurrentSoreness :: forall m. (MonadIO m) => Connection -> m [Soreness]
retrieveCurrentSoreness connection = do
  -- Courtesy of
  -- https://stackoverflow.com/questions/17327043/how-can-i-select-rows-with-most-recent-timestamp-for-each-key-value
  results <-
    liftIO $
      query_
        connection
        "SELECT \
        \ M.name, L.muscle_id, L.soreness, L.time \
        \ FROM Soreness L \
        \ LEFT JOIN Soreness R ON L.muscle_id = R.muscle_id AND L.time < R.time \
        \ INNER JOIN Muscle M ON M.id == L.muscle_id \
        \ WHERE R.muscle_id IS NULL AND L.soreness > 0" ::
      m [(Text, IdType, Int, UTCTime)]

  pure ((\(muscleName, muscleId, soreness, time) -> Soreness muscleId muscleName soreness time) <$> results)

-- Calculate all soreness values and return as a flat list - the user
-- can filter group as they see fit
retrieveSorenessHistory :: forall m. (MonadIO m) => Connection -> m [Soreness]
retrieveSorenessHistory connection = do
  results <-
    liftIO $
      query_
        connection
        "SELECT \
        \ M.name, L.muscle_id, L.soreness, L.time \
        \ FROM Soreness L \
        \ INNER JOIN Muscle M ON M.id == L.muscle_id" ::
      m [(Text, IdType, Int, UTCTime)]

  pure ((\(muscleName, muscleId, soreness, time) -> Soreness muscleId muscleName soreness time) <$> results)

data ExerciseCommitted = Committed | NotCommitted

-- TODO: Add muscle group filter here
retrieveExercisesWithWorkouts :: forall m. (MonadIO m) => Connection -> Maybe ExerciseCommitted -> m [ExerciseWithWorkouts]
retrieveExercisesWithWorkouts conn committedFilterMaybe = liftIO $ do
  let queryBase =
        "SELECT E.id, E.description, E.name, M.id muscle_id, M.name muscle_name, F.id file_id, WI.intensity, WI.time, WI.committed \
        \  FROM Exercise as E \
        \  LEFT JOIN ExerciseHasMuscle ON E.id = ExerciseHasMuscle.exercise_id \
        \  LEFT JOIN Muscle M ON M.id = ExerciseHasMuscle.muscle_id \
        \  LEFT JOIN ExerciseHasFile F ON E.id = F.exercise_id \
        \  LEFT JOIN ExerciseWithIntensity WI ON E.id = WI.exercise_id"
      finalStatement :: IO [(IdType, Text, Text, Maybe IdType, Maybe Text, Maybe IdType, Maybe Text, Maybe UTCTime, Maybe Int)]
      finalStatement = case committedFilterMaybe of
        Nothing -> query_ conn queryBase
        Just committedFilter ->
          query
            conn
            (queryBase <> " WHERE WI.committed = ?")
            (Only @Int (case committedFilter of Committed -> 1; NotCommitted -> 0))

  results <- finalStatement
  exerciseDataRef <- newIORef mempty
  exerciseMusclesByIdRef <- newIORef mempty
  exerciseFilesByIdRef <- newIORef mempty
  exerciseToWorkoutsRef <- newIORef mempty
  forM_ results \(exId, exDescription, exName, muId, muName, fileId, intensity, time, committed) -> do
    insertMSet exerciseDataRef (exId, exDescription, exName)

    for_ ((,) <$> muId <*> muName) (multiInsertMMap exerciseMusclesByIdRef exId . uncurry Muscle)
    for_ fileId (multiInsertMMap exerciseFilesByIdRef exId)

    for_
      ( ( \intensity' time' committed' ->
            ExerciseWorkout
              { exerciseId = exId,
                time = time',
                committed = committed' > 0,
                intensity = intensity'
              }
        )
          <$> intensity
          <*> time
          <*> committed
      )
      (multiInsertMMap exerciseToWorkoutsRef exId)

  exerciseData <- readIORef exerciseDataRef
  exerciseMusclesById <- readIORef exerciseMusclesByIdRef
  exerciseFilesById <- readIORef exerciseFilesByIdRef
  exerciseToWorkouts <- readIORef exerciseToWorkoutsRef

  pure
    ( ( \(exId, exDescription, exName) ->
          ExerciseWithWorkouts
            { id = exId,
              description = exDescription,
              name = exName,
              muscles = fromMaybe mempty (Map.lookup exId exerciseMusclesById),
              fileIds = fromMaybe mempty (Map.lookup exId exerciseFilesById),
              workouts = fromMaybe mempty (Map.lookup exId exerciseToWorkouts)
            }
      )
        <$> Set.toList exerciseData
    )

retrieveAllMuscles :: forall m. (MonadIO m) => Connection -> m [Muscle]
retrieveAllMuscles connection = liftIO do
  results <- query_ connection "SELECT id, name FROM Muscle ORDER BY name ASC" :: IO [(IdType, Text)]
  pure (uncurry Muscle <$> results)

retrieveExercisesDescriptions :: forall m. (MonadIO m) => Connection -> m [ExerciseDescription]
retrieveExercisesDescriptions conn = liftIO do
  results <-
    query_
      conn
      "SELECT E.id, E.description, E.name, M.id muscle_id, M.name muscle_name, F.id file_id \
      \  FROM Exercise as E \
      \  LEFT JOIN ExerciseHasMuscle ON E.id = ExerciseHasMuscle.exercise_id \
      \  LEFT JOIN Muscle M ON M.id = ExerciseHasMuscle.muscle_id \
      \  LEFT JOIN ExerciseHasFile F ON E.id = F.exercise_id \
      \  ORDER BY E.name ASC" ::
      IO [(IdType, Text, Text, Maybe IdType, Maybe Text, Maybe IdType)]

  exerciseDataRef <- newIORef mempty
  exerciseMusclesByIdRef <- newIORef mempty
  exerciseFilesByIdRef <- newIORef mempty
  forM_ results \(exId, exDescription, exName, muId, muName, fileId) -> do
    insertMSet exerciseDataRef (exId, exDescription, exName)

    for_ ((,) <$> muId <*> muName) (multiInsertMMap exerciseMusclesByIdRef exId . uncurry Muscle)
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

-- retrieveExerciseHistoryForMuscle :: Connection -> Muscle -> IO []
retrieveFile :: forall m. (MonadIO m) => Connection -> IdType -> m BSL.ByteString
retrieveFile conn fileId = liftIO (fromOnly . head <$> query conn "SELECT file_content FROM ExerciseHasFile WHERE id = ?" (Only fileId))

uploadFileForExercise :: forall m. (MonadIO m) => Connection -> IdType -> BSL.ByteString -> m IdType
uploadFileForExercise connection exerciseId content = liftIO do
  execute connection "INSERT INTO ExerciseHasFile (exercise_id, file_content) VALUES (?, ?)" (exerciseId, content)
  lastInsertRowId connection

retrieveMusclesWithLastWorkoutTime :: forall m. (MonadIO m) => Connection -> m [MuscleWithWorkout]
retrieveMusclesWithLastWorkoutTime conn = liftIO do
  results <- query_ conn "SELECT EHM.muscle_id, Muscle.name, MAX(EWI.time) FROM ExerciseWithIntensity EWI INNER JOIN ExerciseHasMuscle EHM ON (EWI.exercise_id = EHM.exercise_id) INNER JOIN Muscle ON Muscle.id = EHM.muscle_id GROUP BY EHM.muscle_id" :: IO [(IdType, Text, UTCTime)]
  pure
    ( ( \(muscleId, muscleName, time) ->
          MuscleWithWorkout
            { muscleId = muscleId,
              workoutTime = time,
              muscleName = muscleName
            }
      )
        <$> results
    )

removeExercise :: forall m. (MonadIO m) => Connection -> IdType -> m ()
removeExercise conn exerciseId = liftIO (execute conn "DELETE FROM Exercise WHERE id = ?" (Only exerciseId))

insertMuscle :: forall m. (MonadIO m) => Connection -> Text -> m IdType
insertMuscle conn name = liftIO do
  execute conn "INSERT INTO Muscle (name) VALUES (?)" (Only name)
  lastInsertRowId conn

insertExercise :: forall m. (MonadIO m) => Connection -> Set.Set IdType -> Text -> Text -> [BSL.ByteString] -> m IdType
insertExercise conn muscles name description files = liftIO do
  execute conn "INSERT INTO Exercise (name, description) VALUES (?, ?)" (name, description)
  exerciseId <- lastInsertRowId conn
  forM_ muscles \muscle ->
    execute conn "INSERT INTO ExerciseHasMuscle (exercise_id, muscle_id) VALUES (?, ?)" (exerciseId, muscle)
  forM_ files \file -> execute conn "INSERT INTO ExerciseHasFile (exercise_id, file_content) VALUES (?, ?)" (exerciseId, file)
  pure exerciseId

updateExercise :: forall m. (MonadIO m) => Connection -> IdType -> Set.Set Muscle -> Text -> Text -> [BSL.ByteString] -> [IdType] -> m ()
updateExercise conn exerciseId muscles name description newFiles deleteOldFiles = liftIO do
  execute conn "UPDATE Exercise SET name = ?, description = ? WHERE id = ?" (name, description, exerciseId)
  execute conn "DELETE FROM ExerciseHasMuscle WHERE exercise_id = ?" (Only exerciseId)
  forM_ muscles \muscle ->
    execute conn "INSERT INTO ExerciseHasMuscle (exercise_id, muscle_id) VALUES (?, ?)" (exerciseId, muscle.id)
  forM_ deleteOldFiles \oldFileId ->
    execute conn "DELETE FROM ExerciseHasFile WHERE exercise_id = ? AND file_id = ?" (exerciseId, oldFileId)
  forM_ newFiles \file ->
    execute conn "INSERT INTO ExerciseHasFile (exercise_id, file_content) VALUES (?, ?)" (exerciseId, file)

toggleExercise :: forall m. (MonadIO m) => Connection -> IdType -> UTCTime -> Text -> m ()
toggleExercise conn exerciseId currentTime intensity = liftIO do
  execute conn "DELETE FROM ExerciseWithIntensity WHERE exercise_id = ? AND committed = ?" (exerciseId, 0 :: Int)
  numberOfDeletions <- changes conn
  when
    (numberOfDeletions == 0)
    ( execute
        conn
        "INSERT INTO ExerciseWithIntensity (exercise_id, intensity, time, committed) VALUES (?, ?, ?, ?)"
        (exerciseId, intensity, currentTime, 0 :: Int)
    )

changeIntensity :: forall m. (MonadIO m) => Connection -> IdType -> Text -> m ()
changeIntensity conn exerciseId intensity = liftIO do
  execute conn "UPDATE ExerciseWithIntensity SET intensity = ? WHERE exerciseId = ? AND committed = ?" (intensity, exerciseId, 0 :: Int)

updateSoreness :: forall m. (MonadIO m) => Connection -> IdType -> Int -> UTCTime -> m ()
updateSoreness conn muscleId soreness currentTime = liftIO do
  execute conn "INSERT INTO Soreness (muscle_id, soreness, time) VALUES (?, ?, ?)" (muscleId, soreness, currentTime)

commitWorkout :: forall m. (MonadIO m) => Connection -> m ()
commitWorkout conn = liftIO do
  execute conn "UPDATE ExerciseWithIntensity SET committed = ?" (Only (1 :: Int))
