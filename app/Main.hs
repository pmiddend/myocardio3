{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Applicative (Applicative (pure))
import Control.Monad (forM_, mapM_, void, (>>=))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bifunctor (first)
import Data.Bool (Bool (False, True), (&&))
import Data.ByteString.Lazy qualified as BSL
import Data.Either (fromRight)
import Data.Eq (Eq ((/=)), (==))
import Data.Foldable (find, foldMap)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int, Int64)
import Data.List (filter, replicate, sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (Maybe (Just, Nothing), mapMaybe, maybe)
import Data.Monoid (mempty)
import Data.Ord (comparing, (>=))
import Data.Semigroup (Semigroup ((<>)))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Read (decimal)
import Data.Time (Day)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Traversable (sequence, traverse)
import Lucid (renderText)
import Myocardio.AbsoluteWeek (beginningOfAbsoluteWeeks, getCurrentAbsoluteWeek)
import Myocardio.DatabaseNew
  ( ExerciseCommitted (NotCommitted),
    ExerciseDescription (ExerciseDescription, description, fileIds, id, muscles, name),
    ExerciseWithWorkouts (id, workouts),
    ExerciseWorkout (intensity, time),
    IdType,
    Muscle (id),
    MuscleWithWorkoutWeek (MuscleWithWorkoutWeek, muscle, week),
    SorenessScalar (LittleSore, NotSore, VerySore),
    changeIntensity,
    commitWorkout,
    insertExercise,
    removeExercise,
    retrieveAllMuscles,
    retrieveCurrentSoreness,
    retrieveExercisesDescriptions,
    retrieveExercisesWithWorkouts,
    retrieveFile,
    retrieveLastWorkout,
    retrieveMusclesTrainedHistory,
    retrieveMusclesWithDates,
    retrieveSorenessHistory,
    retrieveWorkoutsPerWeek,
    toggleExercise,
    updateExercise,
    updateSoreness,
    withDatabase,
  )
import Myocardio.Statistics (histogramForWorkouts, regressionForWorkouts, viewChartForWorkouts)
import Network.HTTP.Types.Status (status400)
import Network.Wai.Middleware.Static (addBase, isNotAbsolute, noDots, staticPolicy)
import Network.Wai.Parse (FileInfo (fileContent, fileName))
import Safe.Foldable (maximumByMay)
import System.IO (FilePath, IO)
import Util (packShowLazy)
import Views (exerciseFormDescriptionParam, exerciseFormFilesToDeleteParam, exerciseFormMusclesParam, exerciseFormNameParam, muscleIdForMuscleSorenessFromHtml, viewChooseOuter, viewConcreteMuscleGroupExercisesOuter, viewExerciseDeletion, viewExerciseListOuter, viewPageCurrentHtml, viewStats)
import Web.Scotty (ActionM, Parsable (parseParam, parseParamList), capture, files, finish, formParam, formParamMaybe, formParams, get, html, pathParam, post, queryParamMaybe, raw, redirect, scotty, setHeader, status, text)
import Web.Scotty.Trans (middleware)
import Prelude (Double, Either (Left, Right), (-))

instance Parsable Day where
  parseParam v =
    case iso8601ParseM (TL.unpack v) of
      Nothing -> Left ("cannot parse form date parameter " <> v)
      Just day -> Right day

instance (Parsable a) => Parsable (NE.NonEmpty a) where
  parseParam v =
    case parseParamList v of
      Left e -> Left e
      Right v' -> case NE.nonEmpty v' of
        Nothing -> Left "list with no elements"
        Just nonEmptyList -> Right nonEmptyList

paramValues :: forall a. (Parsable a) => Text -> ActionM (Either Text [a])
paramValues desiredParamName = do
  formParams' <- formParams
  let parsedParams :: [Either TL.Text a]
      parsedParams =
        mapMaybe
          ( \(paramName, paramValue) ->
              if paramName == desiredParamName then Just (parseParam (TL.fromStrict paramValue)) else Nothing
          )
          formParams'
  pure (first TL.toStrict (sequence parsedParams))

paramValuesNE :: (Parsable a) => Text -> ActionM (Either Text (NE.NonEmpty a))
paramValuesNE name = do
  result <- paramValues name
  case result of
    Left e -> pure (Left e)
    Right result' ->
      case NE.nonEmpty result' of
        Nothing -> pure (Left $ "couldn't find parameter \"" <> name <> "\"")
        Just result'' -> pure (Right result'')

-- This path is changed by the Nix build to point to $out
staticBasePath :: FilePath
staticBasePath = "static/"

finishWithBadRequest :: TL.Text -> ActionM a
finishWithBadRequest message = do
  status status400
  text message
  finish

mainPage :: Bool -> ActionM ()
mainPage sorenessWasUpdated = withDatabase \connection -> do
  allMuscles' <- retrieveAllMuscles connection
  exercises <- retrieveExercisesWithWorkouts connection (Just NotCommitted)
  currentSoreness <- retrieveCurrentSoreness connection
  lastWorkout <- retrieveLastWorkout connection
  currentTime <- liftIO getCurrentTime
  musclesLastWeek <- retrieveMusclesTrainedHistory connection 7
  html $ renderText $ viewPageCurrentHtml currentTime allMuscles' exercises lastWorkout currentSoreness musclesLastWeek sorenessWasUpdated

main :: IO ()
main = do
  scotty 3000 do
    get "/" do
      mainPage False

    get "/repeat-last" do
      withDatabase \connection -> do
        lastWorkout <- retrieveLastWorkout connection
        currentTime <- liftIO getCurrentTime
        forM_ lastWorkout \exerciseWithWorkout -> do
          toggleExercise
            connection
            exerciseWithWorkout.id
            currentTime
            (maybe "" (.intensity) (maximumByMay (comparing (.time)) (Set.elems exerciseWithWorkout.workouts)))
        allMuscles' <- retrieveAllMuscles connection
        exercises <- retrieveExercisesWithWorkouts connection (Just NotCommitted)
        currentSoreness <- retrieveCurrentSoreness connection
        musclesLastWeek <- retrieveMusclesTrainedHistory connection 7
        html $ renderText $ viewPageCurrentHtml currentTime allMuscles' exercises lastWorkout currentSoreness musclesLastWeek False

    get "/exercises" do
      withDatabase \connection -> do
        exercises <- sortOn (.name) <$> retrieveExercisesDescriptions connection
        allMuscles' <- retrieveAllMuscles connection
        withForm <- queryParamMaybe "with-form"
        editExerciseId <- queryParamMaybe "edit-exercise"
        case editExerciseId of
          Nothing ->
            html $
              renderText $
                viewExerciseListOuter
                  allMuscles'
                  exercises
                  ( if withForm == Just True
                      then
                        Just
                          ExerciseDescription
                            { id = 0,
                              muscles = mempty,
                              description = "",
                              name = "",
                              fileIds = mempty
                            }
                      else Nothing
                  )
          Just editExerciseId' ->
            case find (\e -> e.id == editExerciseId') exercises of
              Nothing -> do
                status status400
                finish
              Just exerciseFound ->
                html $
                  renderText $
                    viewExerciseListOuter allMuscles' exercises (Just exerciseFound)

    get (capture "/training") do
      withDatabase \connection -> do
        allMuscles' <- retrieveAllMuscles connection
        exercises <- retrieveExercisesWithWorkouts connection (Just NotCommitted)
        currentSoreness <- retrieveCurrentSoreness connection
        html $ renderText $ viewChooseOuter allMuscles' currentSoreness exercises

    get (capture "/training/:muscleid") do
      withDatabase \connection -> do
        currentTime <- liftIO getCurrentTime
        muscleId <- pathParam "muscleid"
        allMuscles' <- retrieveAllMuscles connection
        exercises <- retrieveExercisesWithWorkouts connection Nothing
        sorenessHistory <- retrieveSorenessHistory connection
        currentSoreness <- retrieveCurrentSoreness connection

        case find (\m -> m.id == muscleId) allMuscles' of
          Nothing -> do
            status status400
            text ("I couldn't parse the muscle you gave me: " <> packShowLazy muscleId)
            finish
          Just muscle ->
            html $ renderText $ viewConcreteMuscleGroupExercisesOuter currentTime sorenessHistory currentSoreness exercises muscle

    get "/uploaded-files/:fileid" do
      withDatabase \connection -> do
        fileId <- pathParam "fileid"
        file' <- retrieveFile connection fileId
        raw file'

    get "/remove-exercise/:exercise-id" do
      exerciseId <- pathParam "exercise-id"
      -- Explicit annotation because it could be Text, LazyText, String, ...
      sure :: Maybe Text <- queryParamMaybe "sure"
      withDatabase \connection ->
        if sure == Just "yes"
          then do
            removeExercise connection exerciseId
            redirect "/"
          else do
            exercises <- retrieveExercisesDescriptions connection
            case find (\e -> e.id == exerciseId) exercises of
              Nothing -> redirect "/"
              Just (ExerciseDescription {name}) -> html $ renderText $ viewExerciseDeletion exerciseId name

    post "/edit-exercise" do
      -- Very very weird behavior - why is there always at least one file, with not even an empty
      -- file name but ""?
      uploadedFiles :: [(Text, FileInfo BSL.ByteString)] <- filter (\(_, fileData) -> fileName fileData /= "\"\"") <$> files
      musclesRaw :: Either Text (NE.NonEmpty IdType) <- paramValuesNE exerciseFormMusclesParam
      case musclesRaw of
        Left parseError -> do
          status status400
          text ("I couldn't parse all the muscle IDs you gave me, or the list was empty: " <> packShowLazy parseError)
          finish
        Right muscleIds -> withDatabase \conn -> do
          allMuscles <- retrieveAllMuscles conn
          case traverse (\muscleId -> find (\m -> m.id == muscleId) allMuscles) muscleIds of
            Nothing -> finishWithBadRequest ("one of the muscles was not found, id list is " <> packShowLazy muscleIds)
            Just muscles -> do
              description' <- formParam exerciseFormDescriptionParam
              name' <- formParam exerciseFormNameParam
              toDeleteEither <- paramValues exerciseFormFilesToDeleteParam
              case toDeleteEither of
                Left _ -> finishWithBadRequest "one of the IDs to delete was not found"
                Right toDelete -> do
                  exerciseId <- formParamMaybe "exercise-id"
                  let uploadedFileContents :: [BSL.ByteString]
                      uploadedFileContents = (\(_, fileInfo) -> fileContent fileInfo) <$> uploadedFiles
                  case exerciseId of
                    Nothing ->
                      void (insertExercise conn (foldMap (Set.singleton . (.id)) muscles) name' description' uploadedFileContents)
                    Just exerciseId' ->
                      updateExercise
                        conn
                        exerciseId'
                        (foldMap Set.singleton muscles)
                        name'
                        description'
                        uploadedFileContents
                        toDelete
                  redirect "/exercises"

    post "/toggle-exercise-in-workout" do
      exerciseId <- formParam "exercise-id"
      intensity' <- formParam "intensity"

      currentTime <- liftIO getCurrentTime
      withDatabase \conn -> toggleExercise conn exerciseId currentTime intensity'
      returnToCurrent :: Maybe Bool <- formParamMaybe "return-to-current"
      redirect case returnToCurrent of
        Nothing ->
          TL.fromStrict "/training"
        Just _ -> "/"

    post "/change-intensity" do
      exerciseId <- formParam "exercise-id"
      intensity' <- formParam "intensity"
      withDatabase \conn -> changeIntensity conn exerciseId intensity'
      redirect "/"

    post "/update-soreness" do
      allParameters <- formParams
      let sorenessParams :: [(Int64, SorenessScalar)]
          sorenessParams =
            mapMaybe
              ( \(name, soreness) ->
                  case muscleIdForMuscleSorenessFromHtml name of
                    Nothing -> Nothing
                    Just muscleId ->
                      case decimal @Int soreness of
                        Left _ -> Nothing
                        Right (sorenessInt, _) ->
                          if sorenessInt == 0
                            then Just (muscleId, NotSore)
                            else
                              if sorenessInt == 1
                                then Just (muscleId, LittleSore)
                                else Just (muscleId, VerySore)
              )
              allParameters

      currentTime <- liftIO getCurrentTime
      withDatabase \conn -> do
        mapM_ (\(muscleId, soreness) -> updateSoreness conn muscleId soreness currentTime) sorenessParams
      mainPage True

    post "/commit-workout" do
      workoutDate <- formParamMaybe "workout-date"
      withDatabase (commitWorkout workoutDate)
      redirect "/"

    get "/stats/overall" do
      withDatabase retrieveAllMuscles >>= \case
        [] -> html $ renderText "no muscles yet"
        firstMuscle : _ -> do
          currentWeek <- getCurrentAbsoluteWeek
          workoutsPerWeek <- withDatabase retrieveWorkoutsPerWeek
          chart <-
            viewChartForWorkouts
              (beginningOfAbsoluteWeeks, currentWeek)
              ( foldMap
                  (\(week, count) -> replicate count (MuscleWithWorkoutWeek firstMuscle week))
                  (fromRight [] workoutsPerWeek)
              )
          setHeader "Content-Type" "image/png"
          raw chart

    get "/stats/:muscleid" do
      muscleId <- pathParam "muscleid"
      musclesWithDate <- withDatabase retrieveMusclesWithDates
      currentWeek <- getCurrentAbsoluteWeek
      chart <-
        viewChartForWorkouts
          (beginningOfAbsoluteWeeks, currentWeek)
          (filter (\mwd -> mwd.muscle.id == muscleId) musclesWithDate)

      setHeader "Content-Type" "image/png"
      raw chart

    get "/stats" do
      musclesWithDate <- withDatabase retrieveMusclesWithDates

      withDatabase retrieveAllMuscles >>= \case
        [] -> html $ renderText "no muscles yet"
        allMuscles -> do
          currentWeek <- getCurrentAbsoluteWeek

          let regressionForMuscle muscle =
                regressionForWorkouts
                  ( histogramForWorkouts
                      (currentWeek - 8, currentWeek)
                      ( filter
                          (\mwd -> mwd.muscle.id == muscle.id && (mwd.week >= currentWeek - 8))
                          musclesWithDate
                      )
                  )
              muscleToRegression :: Map.Map Muscle (Double, Double)
              muscleToRegression = foldMap (\muscle -> Map.singleton muscle (regressionForMuscle muscle)) allMuscles

          html $ renderText (viewStats allMuscles muscleToRegression)

    middleware (staticPolicy (noDots <> isNotAbsolute <> addBase staticBasePath))
