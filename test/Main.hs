{-# LANGUAGE BlockArguments #-}

module Main (main) where

import Control.Applicative (pure)
import Control.Exception (bracket)
import Data.Eq ((==))
import Data.Fixed (Pico)
import Data.Foldable (find)
import Data.Function (($))
import Data.List (length)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Monoid (mempty)
import Data.Set qualified as Set
import Data.Time.Clock (NominalDiffTime, addUTCTime, getCurrentTime, secondsToNominalDiffTime)
import Myocardio.DatabaseNew (Connection, MigrationFlags (NoJson), SorenessScalar (LittleSore, NotSore), closeDatabase, id, insertExercise, insertMuscle, migrateDatabase, muscleId, openDatabase, retrieveAllMuscles, retrieveCurrentSoreness, retrieveExercisesDescriptions, retrieveExercisesWithWorkouts, retrieveMusclesWithLastWorkoutTime, retrieveSorenessHistory, toggleExercise, updateSoreness, workouts)
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
          [sorenessValue] -> do
            sorenessValue.muscleId @?= pecs
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

        allMuscles <- retrieveAllMuscles conn

        length allMuscles @?= 2

        musclesAndLastWorkout <- retrieveMusclesWithLastWorkoutTime conn
        length musclesAndLastWorkout @?= 0

        step "Creating exercises"
        ex1 <- insertExercise conn (Set.fromList [pecs, core]) "ex1" "some description" mempty
        _ex2 <- insertExercise conn (Set.fromList [pecs, core]) "ex2" "some description" mempty

        exercises <- retrieveExercisesDescriptions conn
        length exercises @?= 2

        exercisesAndWorkouts <- retrieveExercisesWithWorkouts conn Nothing
        length exercisesAndWorkouts @?= 2

        step "Adding to workout"
        currentTime <- getCurrentTime

        toggleExercise conn ex1 currentTime "intense!"
        exercisesAndWorkouts' <- retrieveExercisesWithWorkouts conn Nothing

        case find (\eaw -> eaw.id == ex1) exercisesAndWorkouts' of
          Nothing -> assertFailure "couldn't find ex1 in exercise list"
          Just ex1WithWorkouts ->
            Set.size ex1WithWorkouts.workouts @?= 1

            -- contentsAfterToggle <- readDatabase dbFile
            -- -- be sure our data is still there
            -- length contentsAfterToggle.exercises @?= 2
            -- length contentsAfterToggle.currentTraining @?= 1
            -- length contentsAfterToggle.pastExercises @?= 0

            -- case headMay contentsAfterToggle.currentTraining of
            --   Just exWithIn -> do
            --     exWithIn.exercise.name @?= ExerciseName "ex1"
            --     exWithIn.intensity @?= Intensity "intense!"
            --   Nothing -> assertFailure "didn't encounter just-added exercise in workout"

            -- step "Committing workout"
            -- commitWorkout dbFile

            -- contentsAfterCommit <- readDatabase dbFile
            -- -- be sure our data is still there
            -- length contentsAfterCommit.exercises @?= 2
            -- length contentsAfterCommit.currentTraining @?= 0
            -- length contentsAfterCommit.pastExercises @?= 1,
            -- testCaseSteps "add exercises, remove one of them" $ \step -> withTemporaryDb \dbFile -> do
            --   step "Creating exercises"
            --   addExercise dbFile $
            --     Exercise
            --       { muscles = NE.fromList [Pecs, Core],
            --         category = Strength,
            --         description = "some description",
            --         fileReferences = [],
            --         name = ExerciseName "ex1"
            --       }
            --   addExercise dbFile $
            --     Exercise
            --       { muscles = NE.fromList [Pecs, Core],
            --         category = Strength,
            --         description = "some description",
            --         fileReferences = [],
            --         name = ExerciseName "ex2"
            --       }
            --   step "Removing the first one"
            --   removeExercise dbFile (ExerciseName "ex1")
            --   contentsAfterRemoval <- readDatabase dbFile

            --   case contentsAfterRemoval.exercises of
            --     [] -> assertFailure "no exercises left after removal"
            --     [e] -> e.name @?= ExerciseName "ex2"
            --     _ -> assertFailure "too many exercises left after removal",
            -- testCaseSteps "add exercise, work them out, then have DOMS directly after" $
            --   \step -> withTemporaryDb \dbFile -> do
            --     step "Creating exercise"

            --     let exName = ExerciseName "ex1"

            --     addExercise dbFile $
            --       Exercise
            --         { muscles = NE.fromList [Pecs, Core],
            --           category = Strength,
            --           description = "some description",
            --           fileReferences = [],
            --           name = exName
            --         }

            --     step "Working out"
            --     workoutDate <- getCurrentTime
            --     toggleExercise dbFile workoutDate (ExerciseName "ex1") (Just (Intensity "intense!"))
            --     commitWorkout dbFile

            --     step "Adding DOMS a day later"

            --     let aDayLater = addUTCTime (days 1) workoutDate
            --         muchLater = addUTCTime (days 4) workoutDate

            --     -- Pecs are worked out as seen above
            --     addSoreness dbFile aDayLater Pecs VerySore

            --     -- Remove soreness later
            --     addSoreness dbFile muchLater Pecs NotSore

            --     -- Assert that we are shown "sore after workout" in the exercise view
            --     readDatabase dbFile >>= \db ->
            --       case firstSorenessBetweenExecutions db Pecs (ExerciseName "ex1") of
            --         Nothing -> assertFailure "no soreness, but expected to be very sore"
            --         Just sorenessWithMeta -> sorenessWithMeta.soreness @?= VerySore,
            -- testCaseSteps "add exercise, work them out, then have DOMS a bit delayed" $
            --   \step -> withTemporaryDb \dbFile -> do
            --     step "Creating exercise"

            --     let exName = ExerciseName "ex1"

            --     addExercise dbFile $
            --       Exercise
            --         { muscles = NE.fromList [Pecs, Core],
            --           category = Strength,
            --           description = "some description",
            --           fileReferences = [],
            --           name = exName
            --         }

            --     step "Working out"
            --     workoutDate <- getCurrentTime
            --     toggleExercise dbFile workoutDate (ExerciseName "ex1") (Just (Intensity "intense!"))
            --     commitWorkout dbFile

            --     step "Adding DOMS two days later"

            --     let twoDaysLater = addUTCTime (days 2) workoutDate
            --         muchLater = addUTCTime (days 4) workoutDate

            --     -- Pecs are worked out as seen above
            --     addSoreness dbFile twoDaysLater Pecs VerySore

            --     -- Remove soreness later
            --     addSoreness dbFile muchLater Pecs NotSore

            --     -- Assert that we are shown "sore after workout" in the exercise view
            --     readDatabase dbFile >>= \db ->
            --       case firstSorenessBetweenExecutions db Pecs (ExerciseName "ex1") of
            --         Nothing -> assertFailure "no soreness, but expected to be very sore"
            --         Just sorenessWithMeta -> sorenessWithMeta.soreness @?= VerySore
    ]

main :: IO ()
main = defaultMain tests
