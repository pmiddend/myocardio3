{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Applicative (Applicative (pure))
import Control.Monad (foldM, forM_, mapM_, void, (>>=))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bifunctor (Bifunctor (second), first)
import Data.Bool (Bool (True))
import Data.ByteString.Lazy qualified as BSL
import Data.Eq (Eq ((/=)), (==))
import Data.Foldable (find, foldMap, foldr, sum)
import Data.Function (($), (.))
import Data.Functor (fmap, (<$>))
import Data.Int (Int, Int64)
import Data.List (drop, filter, scanl', sortOn, splitAt, zip, zipWith)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (Maybe (Just, Nothing), mapMaybe, maybe)
import Data.Monoid (mempty)
import Data.Ord (comparing)
import Data.Semigroup (Semigroup ((<>)))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Data.Text.Read (decimal)
import Data.Time (DayOfWeek (Monday), utctDay)
import Data.Time.Calendar.WeekDate (FirstWeekType (FirstMostWeek), toWeekCalendar)
import Data.Time.Clock (getCurrentTime)
import Data.Traversable (sequence, traverse)
import Data.Tuple (fst, snd)
import Graphics.Rendering.Chart.Backend.Cairo (FileFormat (SVG), toFile, _fo_format, _fo_size)
import Graphics.Rendering.Chart.Easy (def, line, plot)
import Lucid (renderText)
import Myocardio.DatabaseNew
  ( ExerciseCommitted (NotCommitted),
    ExerciseDescription (ExerciseDescription, description, fileIds, id, muscles, name),
    ExerciseWithWorkouts (id, workouts),
    ExerciseWorkout (intensity, time),
    IdType,
    Muscle (id),
    MuscleWithWorkoutTime (MuscleWithWorkoutTime, muscleId),
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
    toggleExercise,
    updateExercise,
    updateSoreness,
    withDatabase,
  )
import Network.HTTP.Types.Status (status400)
import Network.Wai.Middleware.Static (addBase, isNotAbsolute, noDots, staticPolicy)
import Network.Wai.Parse (FileInfo (fileContent, fileName))
import Safe.Foldable (maximumByMay)
import System.IO (FilePath, IO)
import System.IO.Temp (withSystemTempDirectory)
import Util (packShowLazy)
import Views (exerciseFormDescriptionParam, exerciseFormFilesToDeleteParam, exerciseFormMusclesParam, exerciseFormNameParam, muscleIdForMuscleSorenessFromHtml, viewChooseOuter, viewConcreteMuscleGroupExercisesOuter, viewExerciseDeletion, viewExerciseListOuter, viewPageCurrentHtml, viewStats)
import Web.Scotty (ActionM, Parsable (parseParam, parseParamList), capture, files, finish, formParam, formParamMaybe, formParams, get, html, pathParam, post, queryParamMaybe, raw, redirect, scotty, status, text)
import Web.Scotty.Trans (middleware)
import Prelude (Either (Left, Right), Float, Fractional, Integer, fromIntegral, (*), (+), (-), (/))

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

mavg :: (Fractional b) => Int -> [b] -> [b]
mavg k lst = fmap (/ fromIntegral k) $ scanl' (+) (sum h) $ zipWith (-) t lst
  where
    (h, t) = splitAt k lst

viewSvgForMuscle :: (MonadIO m) => [MuscleWithWorkoutTime] -> IdType -> m TL.Text
viewSvgForMuscle musclesWithDate muscleId =
  case filter (\mwd -> mwd.muscleId == muscleId) musclesWithDate of
    [] -> pure mempty
    myMuscle -> do
      currentTime <- liftIO getCurrentTime
      let (currentYear, currentWeek, _) = toWeekCalendar FirstMostWeek Monday (utctDay currentTime)
          alterer :: Maybe Int -> Maybe Int
          alterer Nothing = Just 1
          alterer (Just count) = Just (count + 1)
          weekToCountMap :: Map.Map Integer Int
          weekToCountMap =
            foldr
              ( \(MuscleWithWorkoutTime _muscleId _muscleName year week) ->
                  Map.alter alterer ((fromIntegral year - 2024) * 52 + fromIntegral week)
              )
              ( Map.fromList
                  ( [2024 .. currentYear]
                      >>= ( \year ->
                              (\week -> ((year - 2024) * 52 + week, 0))
                                <$> [1 .. (if year == currentYear then fromIntegral currentWeek else 52)]
                          )
                  )
              )
              myMuscle
          weekToCountOriginal :: [(Integer, Float)]
          weekToCountOriginal = second fromIntegral <$> Map.toList weekToCountMap
          averaged = mavg 3 (snd <$> weekToCountOriginal)
          weekToCount = zip (fst <$> weekToCountOriginal) averaged
      fileContents <- liftIO $ withSystemTempDirectory "chart" \tempDirPath -> do
        let filePath = tempDirPath <> "/chart.png"
        toFile (def {_fo_format = SVG, _fo_size = (400, 300)}) filePath do
          -- layout_title .= unpack mwd.muscleName
          -- layout_left_axis . laxis_override .= axisGridHide
          plot (line "Frequency" [weekToCount])
        BSL.readFile filePath
      let contentsAsText = TLE.decodeUtf8 fileContents
          withoutFirstLine = drop 1 (TL.lines contentsAsText)
      pure (TL.unlines withoutFirstLine)

main :: IO ()
main = do
  scotty 3000 do
    get "/" do
      withDatabase \connection -> do
        allMuscles' <- retrieveAllMuscles connection
        exercises <- retrieveExercisesWithWorkouts connection (Just NotCommitted)
        currentSoreness <- retrieveCurrentSoreness connection
        lastWorkout <- retrieveLastWorkout connection
        currentTime <- liftIO getCurrentTime
        musclesLastWeek <- retrieveMusclesTrainedHistory connection 7
        html $ renderText $ viewPageCurrentHtml currentTime allMuscles' exercises lastWorkout currentSoreness musclesLastWeek

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
        html $ renderText $ viewPageCurrentHtml currentTime allMuscles' exercises lastWorkout currentSoreness musclesLastWeek

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

        case find (\m -> m.id == muscleId) allMuscles' of
          Nothing -> do
            status status400
            text ("I couldn't parse the muscle you gave me: " <> packShowLazy muscleId)
            finish
          Just muscle ->
            html $ renderText $ viewConcreteMuscleGroupExercisesOuter currentTime sorenessHistory exercises muscle

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
        redirect "/"

    post "/commit-workout" do
      withDatabase commitWorkout
      redirect "/"

    get "/stats" do
      musclesWithDate <- withDatabase retrieveMusclesWithDates
      allMuscles <- withDatabase retrieveAllMuscles
      muscleToSvg <-
        foldM
          ( \prevMap newMuscle -> do
              svg <- viewSvgForMuscle musclesWithDate newMuscle.id
              pure (Map.insert newMuscle svg prevMap)
          )
          mempty
          allMuscles
      html $ renderText do
        viewStats muscleToSvg

    middleware (staticPolicy (noDots <> isNotAbsolute <> addBase staticBasePath))
