{-# LANGUAGE BlockArguments #-}

module Main (main) where

import Control.Applicative (pure)
import Control.Exception (bracket)
import Control.Monad ((>>=))
import Data.Fixed (Pico)
import Data.Function (const, ($))
import Data.List (length)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (Maybe (Just, Nothing))
import Data.Time.Clock (NominalDiffTime, addUTCTime, getCurrentTime, secondsToNominalDiffTime)
import Myocardio.Database
  ( Category (Strength),
    DatabaseF (currentTraining, exercises, pastExercises),
    Exercise
      ( Exercise,
        category,
        description,
        fileReferences,
        muscles,
        name
      ),
    ExerciseName (ExerciseName),
    ExerciseWithIntensity (exercise, intensity),
    Intensity (Intensity),
    Muscle (Core, Pecs),
    Soreness (soreness),
    SorenessValue (NotSore, VerySore),
    addExercise,
    addSoreness,
    commitWorkout,
    firstSorenessBetweenExecutions,
    readDatabase,
    removeExercise,
    toggleExercise,
  )
import Safe (headMay)
import System.Directory (removeFile)
import System.FilePath (FilePath)
import System.IO (IO)
import System.IO.Temp (emptySystemTempFile)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, testCaseSteps, (@?=))
import Prelude (Num ((*)))

tests :: TestTree
tests = testGroup "Tests" [unitTests]

days :: Pico -> NominalDiffTime
days i = secondsToNominalDiffTime (i * 86400)

withTemporaryDb :: (FilePath -> IO c) -> IO c
withTemporaryDb f =
  let create = do
        fn <- emptySystemTempFile "myocardio.db"
        removeFile fn
        pure fn
   in bracket create (const (pure ())) f

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ testCase "create DB, should be empty" $ withTemporaryDb \dbFile ->
        do
          contents <- readDatabase dbFile
          contents.currentTraining @?= [],
      testCaseSteps "add exercise, work them out" $ \step -> withTemporaryDb \dbFile -> do
        step "Creating exercises"
        addExercise dbFile $
          Exercise
            { muscles = NE.fromList [Pecs, Core],
              category = Strength,
              description = "some description",
              fileReferences = [],
              name = ExerciseName "ex1"
            }
        addExercise dbFile $
          Exercise
            { muscles = NE.fromList [Pecs, Core],
              category = Strength,
              description = "some description",
              fileReferences = [],
              name = ExerciseName "ex2"
            }
        contentsBefore <- readDatabase dbFile
        length contentsBefore.exercises @?= 2

        step "Adding to workout"
        currentTime <- getCurrentTime
        toggleExercise dbFile currentTime (ExerciseName "ex1") (Just (Intensity "intense!"))

        contentsAfterToggle <- readDatabase dbFile
        -- be sure our data is still there
        length contentsAfterToggle.exercises @?= 2
        length contentsAfterToggle.currentTraining @?= 1
        length contentsAfterToggle.pastExercises @?= 0

        case headMay contentsAfterToggle.currentTraining of
          Just exWithIn -> do
            exWithIn.exercise.name @?= ExerciseName "ex1"
            exWithIn.intensity @?= Intensity "intense!"
          Nothing -> assertFailure "didn't encounter just-added exercise in workout"

        step "Committing workout"
        commitWorkout dbFile

        contentsAfterCommit <- readDatabase dbFile
        -- be sure our data is still there
        length contentsAfterCommit.exercises @?= 2
        length contentsAfterCommit.currentTraining @?= 0
        length contentsAfterCommit.pastExercises @?= 1,
      testCaseSteps "add exercises, remove one of them" $ \step -> withTemporaryDb \dbFile -> do
        step "Creating exercises"
        addExercise dbFile $
          Exercise
            { muscles = NE.fromList [Pecs, Core],
              category = Strength,
              description = "some description",
              fileReferences = [],
              name = ExerciseName "ex1"
            }
        addExercise dbFile $
          Exercise
            { muscles = NE.fromList [Pecs, Core],
              category = Strength,
              description = "some description",
              fileReferences = [],
              name = ExerciseName "ex2"
            }
        step "Removing the first one"
        removeExercise dbFile (ExerciseName "ex1")
        contentsAfterRemoval <- readDatabase dbFile

        case contentsAfterRemoval.exercises of
          [] -> assertFailure "no exercises left after removal"
          [e] -> e.name @?= ExerciseName "ex2"
          _ -> assertFailure "too many exercises left after removal",
      testCaseSteps "add exercise, work them out, then have DOMS directly after" $
        \step -> withTemporaryDb \dbFile -> do
          step "Creating exercise"

          let exName = ExerciseName "ex1"

          addExercise dbFile $
            Exercise
              { muscles = NE.fromList [Pecs, Core],
                category = Strength,
                description = "some description",
                fileReferences = [],
                name = exName
              }

          step "Working out"
          workoutDate <- getCurrentTime
          toggleExercise dbFile workoutDate (ExerciseName "ex1") (Just (Intensity "intense!"))
          commitWorkout dbFile

          step "Adding DOMS a day later"

          let aDayLater = addUTCTime (days 1) workoutDate
              muchLater = addUTCTime (days 4) workoutDate

          -- Pecs are worked out as seen above
          addSoreness dbFile aDayLater Pecs VerySore

          -- Remove soreness later
          addSoreness dbFile muchLater Pecs NotSore

          -- Assert that we are shown "sore after workout" in the exercise view
          readDatabase dbFile >>= \db ->
            case firstSorenessBetweenExecutions db Pecs (ExerciseName "ex1") of
              Nothing -> assertFailure "no soreness, but expected to be very sore"
              Just sorenessWithMeta -> sorenessWithMeta.soreness @?= VerySore,
      testCaseSteps "add exercise, work them out, then have DOMS a bit delayed" $
        \step -> withTemporaryDb \dbFile -> do
          step "Creating exercise"

          let exName = ExerciseName "ex1"

          addExercise dbFile $
            Exercise
              { muscles = NE.fromList [Pecs, Core],
                category = Strength,
                description = "some description",
                fileReferences = [],
                name = exName
              }

          step "Working out"
          workoutDate <- getCurrentTime
          toggleExercise dbFile workoutDate (ExerciseName "ex1") (Just (Intensity "intense!"))
          commitWorkout dbFile

          step "Adding DOMS two days later"

          let twoDaysLater = addUTCTime (days 2) workoutDate
              muchLater = addUTCTime (days 4) workoutDate

          -- Pecs are worked out as seen above
          addSoreness dbFile twoDaysLater Pecs VerySore

          -- Remove soreness later
          addSoreness dbFile muchLater Pecs NotSore

          -- Assert that we are shown "sore after workout" in the exercise view
          readDatabase dbFile >>= \db ->
            case firstSorenessBetweenExecutions db Pecs (ExerciseName "ex1") of
              Nothing -> assertFailure "no soreness, but expected to be very sore"
              Just sorenessWithMeta -> sorenessWithMeta.soreness @?= VerySore
    ]

main :: IO ()
main = defaultMain tests
