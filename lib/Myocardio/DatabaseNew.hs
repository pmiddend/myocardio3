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
    ExerciseToggleState (..),
    MuscleWithWorkoutWeek (MuscleWithWorkoutWeek, muscle, week),
    Soreness (..),
    ExerciseCommitted (..),
    MuscleWithWorkout (..),
    MigrationFlags (..),
    IdType,
    SorenessScalar (..),
    DeprecationStatus (..),
    ExerciseInWorkout (..),
    Workout (..),
    retrieveMusclesWithDates,
    retrieveAllMuscles,
    retrieveWorkoutHistory,
    retrieveLastWorkout,
    retrieveMusclesTrainedHistory,
    retrieveExercisesWithWorkouts,
    retrieveExercisesDescriptions,
    retrieveCurrentSoreness,
    retrieveSorenessHistory,
    retrieveMusclesWithLastWorkoutTime,
    retrieveWorkoutsPerWeek,
    retrieveFile,
    sorenessScalarToInt,
    setDeprecation,
    withDatabase,
    migrateDatabase,
    openDatabase,
    closeDatabase,
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
import Control.Monad (mapM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Bool (Bool (False, True))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Either (Either (Left, Right))
import Data.Eq (Eq ((/=), (==)))
import Data.Foldable (forM_, for_)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.IORef (newIORef, readIORef)
import Data.Int (Int, Int64)
import Data.List (head)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (Maybe (Just, Nothing), fromMaybe, listToMaybe)
import Data.Monoid (mempty)
import Data.Ord (Ord ((<=)), (>))
import Data.Semigroup ((<>))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString (fromString), String)
import Data.Text (Text, pack, splitOn, unpack)
import Data.Text.Read (decimal)
import Data.Time (Day, UTCTime (UTCTime), secondsToDiffTime)
import Data.Traversable (traverse)
import Data.Tuple (fst, uncurry)
import Database.SQLite.Simple (Connection, Only (Only, fromOnly), Query, SQLData (SQLInteger), SQLError, ToRow, changes, close, execute, execute_, lastInsertRowId, open, query, query_, withTransaction)
import Database.SQLite.Simple.FromField (FromField (fromField), ResultError (ConversionFailed), fieldData, returnError)
import Database.SQLite.Simple.Ok (Ok (Ok))
import Database.SQLite.Simple.ToField (ToField (toField))
import Myocardio.AbsoluteWeek (AbsoluteWeek, absoluteWeekFromRelative)
import Myocardio.DatabaseOld qualified as DatabaseJson
import Myocardio.DatabaseOld qualified as OldDb
import Myocardio.MapUtils (insertMMap, insertMSet, multiInsertMMap)
import System.Directory (createDirectoryIfMissing)
import System.Environment.XDG.BaseDir (getUserConfigDir, getUserDataDir)
import System.IO (FilePath, IO)
import Text.Show (Show, show)
import UnliftIO.Exception (bracket)
import Prelude (Num, error, (*))

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

data SorenessScalar = NotSore | LittleSore | VerySore deriving (Eq, Show)

instance Ord SorenessScalar where
  NotSore <= _ = True
  LittleSore <= NotSore = False
  LittleSore <= _ = True
  VerySore <= _ = False

instance FromField SorenessScalar where
  fromField f = case fieldData f of
    SQLInteger b ->
      if b == 0
        then Ok NotSore
        else
          if b == 1
            then Ok LittleSore
            else Ok VerySore
    _ -> returnError ConversionFailed f "expecting an SQLInteger column type"

sorenessScalarToInt :: (Num a) => SorenessScalar -> a
sorenessScalarToInt NotSore = 0
sorenessScalarToInt LittleSore = 1
sorenessScalarToInt VerySore = 2

instance ToField SorenessScalar where
  toField NotSore = SQLInteger 0
  toField LittleSore = SQLInteger 1
  toField _ = SQLInteger 2

data Soreness = Soreness
  { muscle :: !Muscle,
    soreness :: !SorenessScalar,
    time :: !UTCTime
  }
  deriving (Show)

data DeprecationStatus = Deprecated | NotDeprecated deriving (Eq, Show, Ord)

instance FromField DeprecationStatus where
  fromField f =
    case fieldData f of
      SQLInteger 0 -> Ok NotDeprecated
      SQLInteger 1 -> Ok Deprecated
      _ -> returnError ConversionFailed f "bool must be 0 or 1"

instance ToField DeprecationStatus where
  toField NotDeprecated = SQLInteger 0
  toField Deprecated = SQLInteger 1

data ExerciseDescription = ExerciseDescription
  { id :: !IdType,
    muscles :: !(Set.Set Muscle),
    description :: !Text,
    name :: !Text,
    fileIds :: !(Set IdType),
    deprecated :: !DeprecationStatus
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
        MigrateFromJson -> do
          migrateFromJson connection
          migrateDatabase connection flags
        NoJson -> do
          liftIO (execute connection "INSERT INTO Version (version) VALUES (?)" (Only (1 :: Int)))
          migrateDatabase connection flags
    Just 1 -> do
      liftIO $ execute_ connection "ALTER TABLE Exercise ADD COLUMN deprecated INTEGER NOT NULL DEFAULT 0"
      liftIO $ execute_ connection "UPDATE Version SET version = 2"
      migrateDatabase connection flags
    Just 2 -> pure ()
    Just v -> error $ "invalid version stored in DB: expected none, 1 or 2, got " <> show v

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

retrieveMusclesTrainedHistory :: forall m. (MonadIO m) => Connection -> Int -> m [Muscle]
retrieveMusclesTrainedHistory connection days = do
  results <-
    liftIO $
      query_
        connection
        (fromString $ "SELECT M.id, M.name, MAX(time) FROM ExerciseWithIntensity EWI INNER JOIN ExerciseHasMuscle EHM ON EHM.exercise_id = EWI.exercise_id INNER JOIN Muscle M ON M.id == EHM.muscle_id WHERE EWI.time > date('now', '-" <> show days <> " day') GROUP BY M.id") ::
      m [(IdType, Text, UTCTime)]

  pure ((\(muscleId, muscleName, _lastWorkout) -> Muscle muscleId muscleName) <$> results)

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
      m [(Text, IdType, SorenessScalar, UTCTime)]

  pure ((\(muscleName, muscleId, soreness, time) -> Soreness (Muscle muscleId muscleName) soreness time) <$> results)

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
      m [(Text, IdType, SorenessScalar, UTCTime)]

  pure ((\(muscleName, muscleId, soreness, time) -> Soreness (Muscle muscleId muscleName) soreness time) <$> results)

data ExerciseCommitted = Committed | NotCommitted

processExercisesWithWorkoutsQueryBase :: Query
processExercisesWithWorkoutsQueryBase =
  "SELECT E.id, E.description, E.name, M.id muscle_id, M.name muscle_name, F.id file_id, WI.intensity, WI.time, WI.committed \
  \  FROM Exercise as E \
  \  LEFT JOIN ExerciseHasMuscle ON E.id = ExerciseHasMuscle.exercise_id \
  \  LEFT JOIN Muscle M ON M.id = ExerciseHasMuscle.muscle_id \
  \  LEFT JOIN ExerciseHasFile F ON E.id = F.exercise_id \
  \  LEFT JOIN ExerciseWithIntensity WI ON E.id = WI.exercise_id"

processExercisesWithWorkouts :: [(IdType, Text, Text, Maybe IdType, Maybe Text, Maybe IdType, Maybe Text, Maybe UTCTime, Maybe Int)] -> IO [ExerciseWithWorkouts]
processExercisesWithWorkouts results = do
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

data ExerciseInWorkout = ExerciseInWorkout
  { exerciseId :: IdType,
    exerciseName :: Text,
    intensity :: Text
  }

data Workout = Workout
  { day :: Day,
    exercises :: NE.NonEmpty ExerciseInWorkout
  }

fst4 :: (a, b, c, d) -> a
fst4 (a, _, _, _) = a

retrieveWorkoutHistory ::
  forall m.
  (MonadIO m) =>
  Connection ->
  Int ->
  m [Workout]
retrieveWorkoutHistory conn days = do
  results :: [(Day, IdType, Text, Text)] <- liftIO $ query_ conn $ fromString $ "SELECT date(WI.time), E.id, E.name, WI.intensity FROM ExerciseWithIntensity WI INNER JOIN Exercise E ON E.id == WI.exercise_id WHERE WI.committed = 1 AND date(WI.time) < date('now') AND date(WI.time) > date('now', '-" <> show days <> " day') ORDER BY wi.time DESC"
  let convertGroup :: NE.NonEmpty (Day, IdType, Text, Text) -> Workout
      convertGroup items = Workout {day = fst4 (NE.head items), exercises = (\(_, exerciseId, exerciseName, intensity) -> ExerciseInWorkout {exerciseId, exerciseName, intensity}) <$> items}
  pure (convertGroup <$> NE.groupBy (\(day, _, _, _) (day', _, _, _) -> day == day') results)

retrieveExercisesWithWorkouts ::
  forall m.
  (MonadIO m) =>
  Connection ->
  Maybe ExerciseCommitted ->
  Maybe DeprecationStatus ->
  m [ExerciseWithWorkouts]
retrieveExercisesWithWorkouts conn committedFilterMaybe deprecationStatus = liftIO $ do
  let finalStatement :: IO [(IdType, Text, Text, Maybe IdType, Maybe Text, Maybe IdType, Maybe Text, Maybe UTCTime, Maybe Int)]
      finalStatement =
        case (committedFilterMaybe, deprecationStatus) of
          (Nothing, Nothing) -> query_ conn processExercisesWithWorkoutsQueryBase
          (Just committedFilter, Nothing) ->
            query
              conn
              (processExercisesWithWorkoutsQueryBase <> " WHERE WI.committed = ?")
              (Only @Int (case committedFilter of Committed -> 1; NotCommitted -> 0))
          (Nothing, Just deprecationFilter) ->
            query
              conn
              (processExercisesWithWorkoutsQueryBase <> " WHERE E.deprecated = ?")
              (Only @Int (case deprecationFilter of Deprecated -> 1; NotDeprecated -> 0))
          (Just committedFilter, Just deprecationFilter) ->
            query
              conn
              (processExercisesWithWorkoutsQueryBase <> " WHERE E.deprecated = ? AND WI.committed = ?")
              ((case deprecationFilter of Deprecated -> 1; NotDeprecated -> 0, case committedFilter of Committed -> 1; NotCommitted -> 0) :: (Int, Int))
  results <- finalStatement
  processExercisesWithWorkouts results

retrieveAllMuscles :: forall m. (MonadIO m) => Connection -> m [Muscle]
retrieveAllMuscles connection = liftIO do
  results <- query_ connection "SELECT id, name FROM Muscle ORDER BY name ASC" :: IO [(IdType, Text)]
  pure (uncurry Muscle <$> results)

retrieveExercisesDescriptions :: forall m. (MonadIO m) => Connection -> m [ExerciseDescription]
retrieveExercisesDescriptions conn = liftIO do
  results <-
    query_
      conn
      "SELECT E.id, E.description, E.name, E.deprecated, M.id muscle_id, M.name muscle_name, F.id file_id \
      \  FROM Exercise as E \
      \  LEFT JOIN ExerciseHasMuscle ON E.id = ExerciseHasMuscle.exercise_id \
      \  LEFT JOIN Muscle M ON M.id = ExerciseHasMuscle.muscle_id \
      \  LEFT JOIN ExerciseHasFile F ON E.id = F.exercise_id \
      \  ORDER BY E.name ASC" ::
      IO [(IdType, Text, Text, DeprecationStatus, Maybe IdType, Maybe Text, Maybe IdType)]

  exerciseDataRef <- newIORef mempty
  exerciseMusclesByIdRef <- newIORef mempty
  exerciseFilesByIdRef <- newIORef mempty
  forM_ results \(exId, exDescription, exName, exDeprecated, muId, muName, fileId) -> do
    insertMSet exerciseDataRef (exId, exDescription, exName, exDeprecated)

    for_ ((,) <$> muId <*> muName) (multiInsertMMap exerciseMusclesByIdRef exId . uncurry Muscle)
    for_ fileId (multiInsertMMap exerciseFilesByIdRef exId)

  exerciseData <- readIORef exerciseDataRef
  exerciseMusclesById <- readIORef exerciseMusclesByIdRef
  exerciseFilesById <- readIORef exerciseFilesByIdRef

  pure
    ( ( \(exId, exDescription, exName, exDeprecated) ->
          ExerciseDescription
            { id = exId,
              description = exDescription,
              name = exName,
              deprecated = exDeprecated,
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

retrieveWorkoutsPerWeek :: forall m. (MonadIO m) => Connection -> m (Either Text [(AbsoluteWeek, Int)])
retrieveWorkoutsPerWeek conn = liftIO do
  results <- query_ conn "SELECT myweek, COUNT(myweek) FROM (SELECT STRFTIME('%Y-%m-%d', time) mydate, STRFTIME('%Y-%W', time) myweek FROM ExerciseWithIntensity EWI GROUP BY mydate) GROUP BY myweek" :: IO [(Text, Int)]
  let parseWeek x = case splitOn "-" x of
        [yearMaybe, weekMaybe] -> (,) <$> (fst <$> decimal yearMaybe) <*> (fst <$> decimal weekMaybe)
        _ -> Left "invalid year-week string"
      parseResultLine (myweek, count) =
        case parseWeek myweek of
          Left e -> Left (pack e)
          Right (year, week) -> Right (absoluteWeekFromRelative year week, count)

  pure (traverse parseResultLine results)

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

setDeprecation :: forall m. (MonadIO m) => Connection -> IdType -> DeprecationStatus -> m ()
setDeprecation conn exerciseId deprecation = liftIO do
  execute conn "UPDATE Exercise SET deprecated = ? WHERE id = ?" (deprecation, exerciseId)

removeExercise :: forall m. (MonadIO m) => Connection -> IdType -> m ()
removeExercise conn exerciseId = liftIO do
  execute conn "DELETE FROM ExerciseHasMuscle WHERE exercise_id = ?" (Only exerciseId)
  execute conn "DELETE FROM ExerciseHasFile WHERE exercise_id = ?" (Only exerciseId)
  execute conn "DELETE FROM ExerciseWithIntensity WHERE exercise_id = ?" (Only exerciseId)
  execute conn "DELETE FROM Exercise WHERE id = ?" (Only exerciseId)

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
    execute conn "DELETE FROM ExerciseHasFile WHERE exercise_id = ? AND id = ?" (exerciseId, oldFileId)
  forM_ newFiles \file ->
    execute conn "INSERT INTO ExerciseHasFile (exercise_id, file_content) VALUES (?, ?)" (exerciseId, file)

data ExerciseToggleState = ExerciseAdded | ExerciseRemoved

toggleExercise :: forall m. (MonadIO m) => Connection -> IdType -> UTCTime -> Text -> m ExerciseToggleState
toggleExercise conn exerciseId currentTime intensity = liftIO do
  execute conn "DELETE FROM ExerciseWithIntensity WHERE exercise_id = ? AND committed = ?" (exerciseId, 0 :: Int)
  numberOfDeletions <- changes conn
  if numberOfDeletions /= 0
    then pure ExerciseRemoved
    else do
      execute
        conn
        "INSERT INTO ExerciseWithIntensity (exercise_id, intensity, time, committed) VALUES (?, ?, ?, ?)"
        (exerciseId, intensity, currentTime, 0 :: Int)
      pure ExerciseAdded

changeIntensity :: forall m. (MonadIO m) => Connection -> IdType -> Text -> m ()
changeIntensity conn exerciseId intensity = liftIO do
  execute conn "UPDATE ExerciseWithIntensity SET intensity = ? WHERE exercise_id = ? AND committed = ?" (intensity, exerciseId, 0 :: Int)

updateSoreness :: forall m. (MonadIO m) => Connection -> IdType -> SorenessScalar -> UTCTime -> m ()
updateSoreness conn muscleId soreness currentTime = liftIO do
  execute conn "INSERT INTO Soreness (muscle_id, soreness, time) VALUES (?, ?, ?)" (muscleId, soreness, currentTime)

commitWorkout :: forall m. (MonadIO m) => Maybe Day -> Connection -> m ()
commitWorkout workoutDate conn = liftIO do
  case workoutDate of
    Nothing -> execute conn "UPDATE ExerciseWithIntensity SET committed = ?" (Only (1 :: Int))
    Just realWorkoutDate ->
      execute
        conn
        "UPDATE ExerciseWithIntensity SET committed = ?, time = ? WHERE committed = 0"
        (1 :: Int, UTCTime realWorkoutDate (secondsToDiffTime (4 * 60 * 60)))

retrieveLastWorkout :: forall m. (MonadIO m) => Connection -> m [ExerciseWithWorkouts]
retrieveLastWorkout conn = liftIO do
  results <- query_ conn (processExercisesWithWorkoutsQueryBase <> " WHERE DATE(time) IN (SELECT DATE(time) FROM ExerciseWithIntensity WHERE committed = 1 ORDER BY time DESC LIMIT 1)")
  processExercisesWithWorkouts results

data MuscleWithWorkoutWeek = MuscleWithWorkoutWeek
  { muscle :: !Muscle,
    week :: !AbsoluteWeek
  }
  deriving (Show)

retrieveMusclesWithDates :: forall m. (MonadIO m) => Connection -> m [MuscleWithWorkoutWeek]
retrieveMusclesWithDates conn = liftIO do
  results <- query_ conn "SELECT EHM.muscle_id, M.name, CAST(STRFTIME('%Y', time) as INTEGER) year, CAST(STRFTIME('%W', time) AS INTEGER) week FROM ExerciseWithIntensity EWI INNER JOIN ExerciseHasMuscle EHM ON EHM.exercise_id = EWI.exercise_id INNER JOIN Muscle M ON M.id == EHM.muscle_id ORDER BY EHM.muscle_id" :: IO [(IdType, Text, Int, Int)]
  pure ((\(muscleId, muscleName, year, week) -> MuscleWithWorkoutWeek (Muscle muscleId muscleName) (absoluteWeekFromRelative year week)) <$> results)
