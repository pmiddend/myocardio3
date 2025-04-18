{-# LANGUAGE BlockArguments #-}

module Main (main) where

import Control.Applicative (pure)
import Control.Exception (bracket)
import Data.Eq ((==))
import Data.Foldable (find)
import Data.Function (($))
import Data.List (length)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Monoid (mempty)
import Data.Set qualified as Set
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Myocardio.DatabaseNew (Connection, ExerciseWithWorkouts (id), MigrationFlags (NoJson), Muscle (id), Soreness (muscle), SorenessScalar (LittleSore, NotSore), closeDatabase, commitWorkout, insertExercise, insertMuscle, migrateDatabase, openDatabase, retrieveAllMuscles, retrieveCurrentSoreness, retrieveExercisesDescriptions, retrieveExercisesWithWorkouts, retrieveLastWorkout, retrieveMusclesWithLastWorkoutTime, retrieveSorenessHistory, toggleExercise, updateSoreness, workouts)
import System.Directory (removeFile)
import System.IO (IO)
import System.IO.Temp (emptySystemTempFile)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCaseSteps, (@?=))
import Prelude ()

tests :: TestTree
tests = testGroup "Tests" [unitTests]

withTemporaryDb :: (Connection -> IO c) -> IO c
withTemporaryDb f =
  let create = do
        fn <- emptySystemTempFile "myocardio.db"
        removeFile fn
        conn <- openDatabase fn
        migrateDatabase conn NoJson
        pure conn
   in bracket create closeDatabase f

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ testCaseSteps "create DB, should be empty" \_step -> withTemporaryDb \conn -> do
        exercisesWithWorkouts <- retrieveExercisesWithWorkouts conn Nothing
        length exercisesWithWorkouts @?= 0,
      testCaseSteps "create muscles, add soreness and retrieve" $ \step -> withTemporaryDb \conn -> do
        step "Creating muscles"
        pecs <- insertMuscle conn "Pecs"
        _ <- insertMuscle conn "Core"
        now <- getCurrentTime
        step "Updating soreness"
        updateSoreness conn pecs LittleSore (addUTCTime (-100) now)
        sorenessValues <- retrieveCurrentSoreness conn
        case sorenessValues of
          [sorenessValue] ->
            let muscle :: Muscle
                muscle = sorenessValue.muscle
             in muscle.id @?= pecs
          _ -> assertFailure "didn't get exactly one soreness value"
        step "Setting muscle to not sore anymore"
        updateSoreness conn pecs NotSore now
        sorenessValues' <- retrieveCurrentSoreness conn
        length sorenessValues' @?= 0
        step "Retrieve soreness history"
        history <- retrieveSorenessHistory conn

        length history @?= 2,
      testCaseSteps "add exercise, work them out" $ \step -> withTemporaryDb \conn -> do
        step "Creating muscles"
        pecs <- insertMuscle conn "Pecs"
        core <- insertMuscle conn "Core"
        forearms <- insertMuscle conn "Forearms"

        allMuscles <- retrieveAllMuscles conn

        length allMuscles @?= 3

        musclesAndLastWorkout <- retrieveMusclesWithLastWorkoutTime conn
        length musclesAndLastWorkout @?= 0

        step "Creating exercises"
        ex1 <- insertExercise conn (Set.fromList [pecs, core]) "bench press" "some description" mempty
        _ex2 <- insertExercise conn (Set.fromList [pecs, core]) "squat" "some description" mempty
        _ex3 <- insertExercise conn (Set.fromList [forearms]) "climbing" "some description" mempty

        exercises <- retrieveExercisesDescriptions conn
        length exercises @?= 3

        exercisesAndWorkouts <- retrieveExercisesWithWorkouts conn Nothing
        length exercisesAndWorkouts @?= 3

        step "Adding to workout"
        currentTime <- getCurrentTime

        toggleExercise conn ex1 currentTime "intense!"
        exercisesAndWorkouts' <- retrieveExercisesWithWorkouts conn Nothing

        case find (\eaw -> eaw.id == ex1) exercisesAndWorkouts' of
          Nothing -> assertFailure "couldn't find ex1 in exercise list"
          Just ex1WithWorkouts ->
            Set.size ex1WithWorkouts.workouts @?= 1

        step "committing"
        commitWorkout Nothing conn

        step "retrieving last workout"
        lastWorkoutExercises <- retrieveLastWorkout conn

        length lastWorkoutExercises @?= 1
    ]

main :: IO ()
main = defaultMain tests
