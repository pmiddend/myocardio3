{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Myocardio.Database
  ( allMuscles,
    Muscle (..),
    Category (..),
    allCategories,
    Exercise (..),
    currentMuscleSoreness,
    FileReference (..),
    ExerciseName (..),
    commitWorkout,
    Intensity (..),
    intensityToText,
    addExercise,
    ExerciseWithIntensity (..),
    ExerciseNameWithIntensity (..),
    SorenessValue (..),
    Soreness (..),
    DatabaseF (..),
    emptyDatabase,
    addSoreness,
    Database,
    exercisesByName,
    getHomeDbFile,
    changeIntensity,
    readDatabase,
    modifyExercise,
    removeExercise,
    retrieveExercise,
    editExercise,
    toggleExercise,
  )
where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict, encodeFile)
import Data.Bool (not)
import Data.Eq ((/=), (==))
import Data.Foldable (Foldable, find)
import Data.Function (($))
import Data.Functor (Functor, (<$>))
import Data.List (filter, sortBy)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Maybe (Maybe (Just, Nothing))
import Data.Ord (comparing)
import Data.Text (Text, pack, unpack)
import Data.Text.IO (putStrLn)
import Data.Time (getCurrentTime)
import Data.Time.Clock (UTCTime)
import Data.Traversable (Traversable (traverse))
import GHC.Generics (Generic)
import Safe (maximumByMay)
import System.Directory (doesFileExist)
import System.Environment.XDG.BaseDir (getUserDataFile)
import System.IO (FilePath)
import Prelude (Applicative (pure), Bounded, Either (Left, Right), Enum, Eq, Ord, Read, Semigroup ((<>)), Show (show), enumFromTo, error, maxBound, minBound)

data Muscle
  = Neck
  | Biceps
  | Triceps
  | Pecs
  | Shoulders
  | UpperBack
  | Core
  | Adductors
  | LowerBack
  | QL
  | GluteusMedius
  | GluteusMaximus
  | HipFlexors
  | Quadriceps
  | Hamstrings
  | Calves
  | TibialisAnterior
  deriving (Show, Eq, Generic, Enum, Bounded, Ord, Read)

instance FromJSON Muscle

instance ToJSON Muscle

allMuscles :: [Muscle]
allMuscles = enumFromTo minBound maxBound

data Category
  = Strength
  | Endurance
  | Stretch
  | Mobility
  deriving (Show, Eq, Generic, Enum, Bounded, Ord, Read)

instance FromJSON Category

instance ToJSON Category

allCategories :: [Category]
allCategories = enumFromTo minBound maxBound

newtype ExerciseName = ExerciseName {getName :: Text} deriving (Eq, Generic, Ord, Read)

instance Show ExerciseName where
  show (ExerciseName n) = unpack n

instance FromJSON ExerciseName

instance ToJSON ExerciseName

newtype FileReference = FileReference {getFileReference :: Text} deriving (Eq, Show, Generic)

instance FromJSON FileReference

instance ToJSON FileReference

data Exercise = Exercise
  { muscles :: !(NE.NonEmpty Muscle),
    category :: !Category,
    description :: !Text,
    name :: !ExerciseName,
    fileReferences :: ![FileReference]
  }
  deriving (Show, Eq, Generic)

instance FromJSON Exercise

instance ToJSON Exercise

newtype Intensity = Intensity {getIntensity :: Text} deriving (Show, Eq, Generic)

intensityToText :: Intensity -> Text
intensityToText = getIntensity

instance FromJSON Intensity

instance ToJSON Intensity

data ExerciseWithIntensity a = ExerciseWithIntensity
  { exercise :: !a,
    intensity :: !Intensity,
    time :: !UTCTime
  }
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance (FromJSON a) => FromJSON (ExerciseWithIntensity a)

instance (ToJSON a) => ToJSON (ExerciseWithIntensity a)

newtype ExerciseNameWithIntensity = ExerciseNameWithIntensity (ExerciseWithIntensity ExerciseName)
  deriving (Show, Eq, Generic)

instance FromJSON ExerciseNameWithIntensity

instance ToJSON ExerciseNameWithIntensity

data SorenessValue
  = VerySore
  | LittleSore
  | NotSore
  deriving (Show, Eq, Generic, Read)

instance FromJSON SorenessValue

instance ToJSON SorenessValue

data Soreness = Soreness
  { time :: !UTCTime,
    muscle :: !Muscle,
    soreness :: !SorenessValue
  }
  deriving (Show, Eq, Generic)

instance FromJSON Soreness

instance ToJSON Soreness

data DatabaseF a = DatabaseF
  { currentTraining :: ![a],
    pastExercises :: ![a],
    sorenessHistory :: ![Soreness],
    exercises :: ![Exercise]
  }
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

emptyDatabase :: DatabaseF a
emptyDatabase = DatabaseF {currentTraining = [], pastExercises = [], sorenessHistory = [], exercises = []}

instance (FromJSON a) => FromJSON (DatabaseF a)

instance (ToJSON a) => ToJSON (DatabaseF a)

exercisesByName :: DatabaseF a -> Map.Map ExerciseName Exercise
exercisesByName d = Map.fromList ((\e -> (e.name, e)) <$> d.exercises)

newtype DatabaseWithExerciseNames = DatabaseWithExerciseNames (DatabaseF ExerciseNameWithIntensity) deriving (Generic)

instance FromJSON DatabaseWithExerciseNames

instance ToJSON DatabaseWithExerciseNames

getHomeDbFile :: (MonadIO m) => m FilePath
getHomeDbFile = liftIO $ getUserDataFile "myocardio3" "myocardio.json"

type Database = DatabaseF (ExerciseWithIntensity Exercise)

type DatabaseFile = FilePath

readDatabase :: (MonadIO m) => DatabaseFile -> m Database
readDatabase dbFile = do
  liftIO $ putStrLn $ "reading from db file " <> pack dbFile
  exists <- liftIO (doesFileExist dbFile)
  if not exists
    then pure emptyDatabase
    else do
      result <- liftIO $ eitherDecodeFileStrict dbFile
      case result of
        Left _ -> error "error decoding DB JSON"
        Right (DatabaseWithExerciseNames v) -> do
          let resolveExercise :: ExerciseWithIntensity ExerciseName -> Maybe (ExerciseWithIntensity Exercise)
              resolveExercise = traverse (`Map.lookup` exercisesByName v)
              resolved :: Maybe (DatabaseF (ExerciseWithIntensity Exercise))
              resolved = traverse (\(ExerciseNameWithIntensity e) -> resolveExercise e) v
          case resolved of
            Nothing -> error "invalid exercise name"
            Just resolved' ->
              pure
                ( resolved'
                    { sorenessHistory = sortBy (comparing (.time)) resolved'.sorenessHistory,
                      pastExercises = sortBy (comparing (.time)) resolved'.pastExercises
                    }
                )

writeDatabase :: (MonadIO m) => DatabaseFile -> Database -> m ()
writeDatabase dbFile v =
  let encodeDb :: DatabaseF (ExerciseWithIntensity Exercise) -> DatabaseWithExerciseNames
      encodeDb db = DatabaseWithExerciseNames $ (\exWithIn -> ExerciseNameWithIntensity ((.name) <$> exWithIn)) <$> db
   in liftIO $ encodeFile dbFile (encodeDb v)

modifyDb :: (MonadIO m) => DatabaseFile -> (Database -> Database) -> m Database
modifyDb dbFile f = do
  db <- readDatabase dbFile
  writeDatabase dbFile (f db)
  pure (f db)

modifyDb' :: (MonadIO m) => DatabaseFile -> (Database -> Database) -> m ()
modifyDb' dbFile f = void (modifyDb dbFile f)

removeExercise :: (MonadIO m) => DatabaseFile -> ExerciseName -> m ()
removeExercise dbFile exerciseName =
  let filterFromList = filter (\e -> e.exercise.name /= exerciseName)
   in modifyDb' dbFile \db' ->
        db'
          { currentTraining = filterFromList db'.currentTraining,
            pastExercises = filterFromList db'.pastExercises,
            exercises = filter (\e -> e.name /= exerciseName) db'.exercises
          }

modifyExercise :: (MonadIO m) => DatabaseFile -> ExerciseName -> (Exercise -> Exercise) -> m ()
modifyExercise dbFile originalExerciseName modifier = modifyDb' dbFile \db ->
  let replaceOld exWithIn =
        if exWithIn.exercise.name == originalExerciseName
          then exWithIn {exercise = modifier exWithIn.exercise}
          else exWithIn
   in db
        { exercises =
            ( \e ->
                if e.name == originalExerciseName
                  then modifier e
                  else e
            )
              <$> db.exercises,
          currentTraining = replaceOld <$> db.currentTraining,
          pastExercises = replaceOld <$> db.pastExercises
        }

editExercise :: (MonadIO m) => DatabaseFile -> ExerciseName -> Exercise -> m ()
editExercise dbFile originalExerciseName newExercise = modifyDb' dbFile \db ->
  let replaceOld exWithIn = if exWithIn.exercise.name == originalExerciseName then exWithIn {exercise = newExercise} else exWithIn
   in db
        { exercises = newExercise : filter (\e -> e.name /= originalExerciseName) db.exercises,
          currentTraining = replaceOld <$> db.currentTraining,
          pastExercises = replaceOld <$> db.pastExercises
        }

addExercise :: (MonadIO m) => DatabaseFile -> Exercise -> m ()
addExercise dbFile = editExercise dbFile (ExerciseName "")

retrieveExercise :: (MonadIO m) => DatabaseFile -> ExerciseName -> m (Maybe Exercise)
retrieveExercise dbFile ename = do
  db <- readDatabase dbFile
  pure (find (\e -> e.name == ename) db.exercises)

toggleExercise :: (MonadIO m) => DatabaseFile -> ExerciseName -> Maybe Intensity -> m ()
toggleExercise dbFile ename intensity' = do
  currentTime <- liftIO getCurrentTime
  modifyDb' dbFile \db ->
    case find (\e -> e.name == ename) db.exercises of
      Nothing -> db
      Just exercise' ->
        case find (\exWithIn -> exWithIn.exercise.name == ename) db.currentTraining of
          Nothing ->
            case intensity' of
              Nothing -> db
              Just intensity'' ->
                db
                  { currentTraining =
                      ExerciseWithIntensity
                        { exercise = exercise',
                          intensity = intensity'',
                          time = currentTime
                        }
                        : db.currentTraining
                  }
          Just _ -> db {currentTraining = filter (\ewi -> ewi.exercise.name /= ename) db.currentTraining}

changeIntensity :: (MonadIO m) => DatabaseFile -> ExerciseName -> Intensity -> m ()
changeIntensity dbFile ename intensity' = modifyDb' dbFile \db ->
  db
    { currentTraining = (\exWithIn -> if exWithIn.exercise.name == ename then exWithIn {intensity = intensity'} else exWithIn) <$> db.currentTraining
    }

addSoreness :: (MonadIO m) => DatabaseFile -> Muscle -> SorenessValue -> m ()
addSoreness dbFile muscle' soreness' = do
  currentTime <- liftIO getCurrentTime
  modifyDb' dbFile \db ->
    db
      { sorenessHistory = Soreness {time = currentTime, muscle = muscle', soreness = soreness'} : db.sorenessHistory
      }

commitWorkout :: (MonadIO m) => DatabaseFile -> m ()
commitWorkout dbFile = modifyDb' dbFile \db -> db {currentTraining = [], pastExercises = db.currentTraining <> db.pastExercises}

currentMuscleSoreness :: Database -> Muscle -> Maybe Soreness
currentMuscleSoreness db muscle =
  case maximumByMay (comparing (.time)) $ filter (\historyEntry -> historyEntry.muscle == muscle) db.sorenessHistory of
    -- If the latest value is not sore, then don't display soreness at all.
    Just (Soreness {soreness = NotSore}) -> Nothing
    otherValue -> otherValue
