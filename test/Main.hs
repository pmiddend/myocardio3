{-# LANGUAGE BlockArguments #-}

module Main (main) where

import Control.Applicative (pure)
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Data.Function (const, ($))
import Data.List (length)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (Maybe (Just, Nothing))
import Myocardio.Database
import Safe (headMay)
import System.Directory (removeFile)
import System.FilePath (FilePath)
import System.IO (IO)
import System.IO.Temp (emptySystemTempFile)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, testCaseSteps, (@?=))

tests :: TestTree
tests = testGroup "Tests" [unitTests]

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
        toggleExercise dbFile (ExerciseName "ex1") (Just (Intensity "intense!"))

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
          _ -> assertFailure "too many exercises left after removal"
    ]

main :: IO ()
main = defaultMain tests
