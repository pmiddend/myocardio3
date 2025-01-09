{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Applicative (Applicative (pure))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bifunctor (first)
import Data.Bool (Bool (True))
import Data.ByteString.Lazy qualified as BSL
import Data.Eq (Eq ((/=)), (==))
import Data.Foldable (find, foldMap)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.List (filter, sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (Maybe (Just, Nothing), mapMaybe)
import Data.Monoid (mempty)
import Data.Semigroup (Semigroup ((<>)))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Time.Clock (getCurrentTime)
import Data.Traversable (sequence, traverse)
import Lucid (renderText)
import Myocardio.DatabaseNew
  ( ExerciseCommitted (NotCommitted),
    ExerciseDescription (ExerciseDescription, description, fileIds, id, muscles, name),
    IdType,
    Muscle (id),
    changeIntensity,
    commitWorkout,
    insertExercise,
    notSore,
    removeExercise,
    retrieveAllMuscles,
    retrieveCurrentSoreness,
    retrieveExercisesDescriptions,
    retrieveExercisesWithWorkouts,
    retrieveFile,
    retrieveSorenessHistory,
    toggleExercise,
    updateExercise,
    updateSoreness,
    withDatabase,
  )
import Network.HTTP.Types.Status (status400)
import Network.Wai.Middleware.Static (addBase, isNotAbsolute, noDots, staticPolicy)
import Network.Wai.Parse (FileInfo (fileContent, fileName))
import System.IO (FilePath, IO)
import Util (packShowLazy)
import Views (exerciseFormDescriptionParam, exerciseFormFilesToDeleteParam, exerciseFormMusclesParam, exerciseFormNameParam, viewChooseOuter, viewConcreteMuscleGroupExercisesOuter, viewExerciseDeletion, viewExerciseListOuter, viewPageCurrentHtml)
import Web.Scotty (ActionM, Parsable (parseParam, parseParamList), capture, files, finish, formParam, formParamMaybe, formParams, get, html, middleware, pathParam, post, queryParamMaybe, raw, redirect, scotty, status, text)
import Prelude (Either (Left, Right))

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

withMuscle :: IdType -> [Muscle] -> (Muscle -> ActionM a) -> ActionM a
withMuscle muscleId allMuscles f =
  case find (\m -> m.id == muscleId) allMuscles of
    Nothing -> finishWithBadRequest ("I couldn't parse the muscle you gave me: " <> packShowLazy muscleId)
    Just muscle -> f muscle

main :: IO ()
main = do
  scotty 3000 do
    get "/" do
      withDatabase \connection -> do
        allMuscles' <- retrieveAllMuscles connection
        exercises <- retrieveExercisesWithWorkouts connection (Just NotCommitted)
        currentSoreness <- retrieveCurrentSoreness connection
        html $ renderText $ viewPageCurrentHtml allMuscles' exercises currentSoreness

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
      muscleId <- formParam "muscle"
      howSore' <- formParam "how-sore"

      withDatabase \conn -> do
        allMuscles <- retrieveAllMuscles conn
        withMuscle muscleId allMuscles \muscle -> do
          currentTime <- liftIO getCurrentTime
          updateSoreness conn muscle.id howSore' currentTime
          redirect "/"

    post "/reset-soreness" do
      muscleId <- formParam "muscle"
      currentTime <- liftIO getCurrentTime

      withDatabase \conn -> do
        updateSoreness conn muscleId notSore currentTime

      redirect "/"

    post "/commit-workout" do
      withDatabase \conn -> commitWorkout conn
      redirect "/"

    middleware (staticPolicy (noDots <> isNotAbsolute <> addBase staticBasePath))
