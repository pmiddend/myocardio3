{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Applicative (Applicative (pure))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Crypto.Hash.SHA256 (hashlazy)
import Data.Bool (Bool (True))
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as BSL
import Data.Eq (Eq ((/=)), (==))
import Data.Foldable (find, forM_, notElem)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.List (filter, lookup)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (Maybe (Just, Nothing), mapMaybe, maybe)
import Data.Monoid (mempty)
import Data.Semigroup (Semigroup ((<>)))
import Data.Text (Text, pack, unpack)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Time.Clock (getCurrentTime)
import Lucid (renderText)
import Myocardio.Database
  ( Category,
    Exercise (Exercise, category, description, fileReferences, muscles, name),
    ExerciseName (ExerciseName),
    FileReference (FileReference),
    Intensity (Intensity),
    Muscle,
    SorenessValue (NotSore),
    addExercise,
    addSoreness,
    changeIntensity,
    commitWorkout,
    getHomeDbFile,
    modifyExercise,
    readDatabase,
    removeExercise,
    toggleExercise,
  )
import Myocardio.DatabaseNew qualified as DBN
import Network.HTTP.Types.Status (status400)
import Network.Wai.Middleware.Static (addBase, isNotAbsolute, noDots, staticPolicy)
import Network.Wai.Parse (FileInfo (fileContent, fileName))
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Environment (getEnvironment)
import System.Environment.XDG.BaseDir (getUserDataDir)
import System.IO (FilePath, IO, putStrLn)
import Util (packShowLazy)
import Views (exerciseFormCategoryParam, exerciseFormDescriptionParam, exerciseFormFilesToDeleteParam, exerciseFormMusclesParam, exerciseFormNameParam, viewChooseOuter, viewConcreteMuscleGroupExercisesOuter, viewExerciseDeletion, viewExerciseListOuter, viewPageCurrentHtml, viewStats)
import Web.Scotty (ActionM, File, Parsable (parseParam, parseParamList), capture, file, files, finish, formParam, formParamMaybe, formParams, get, html, middleware, pathParam, post, queryParamMaybe, raw, readEither, redirect, scotty, status, text)
import Prelude (Either (Left, Right), Traversable (traverse))

instance (Parsable a) => Parsable (NE.NonEmpty a) where
  parseParam v =
    case parseParamList v of
      Left e -> Left e
      Right v' -> case NE.nonEmpty v' of
        Nothing -> Left "list with no elements"
        Just nonEmptyList -> Right nonEmptyList

instance Parsable Muscle where
  parseParam = readEither

instance Parsable Category where
  parseParam = readEither

instance Parsable SorenessValue where
  parseParam = readEither

instance Parsable Intensity where
  parseParam = Right . Intensity . TL.toStrict

instance Parsable ExerciseName where
  parseParam = Right . ExerciseName . TL.toStrict

getUploadedFileDir :: (MonadIO m) => m FilePath
getUploadedFileDir = do
  uploadedFilesBaseDir <- liftIO $ getUserDataDir "myocardio3"
  pure (uploadedFilesBaseDir <> "/uploaded-files")

uploadSingleFile :: (MonadIO m) => File BSL.ByteString -> m FileReference
uploadSingleFile (_, fileInfo) = do
  let fileHashText :: Text
      fileHashText = TE.decodeUtf8 $ Base16.encode (hashlazy (fileContent fileInfo))
  liftIO $ do
    uploadedFileDir <- getUploadedFileDir
    createDirectoryIfMissing True uploadedFileDir
    BSL.writeFile (uploadedFileDir <> "/" <> unpack fileHashText) (fileContent fileInfo)
  pure (FileReference fileHashText)

paramValues :: Text -> ActionM [Text]
paramValues desiredParamName =
  mapMaybe
    (\(paramName, paramValue) -> if paramName == desiredParamName then Just paramValue else Nothing)
    <$> formParams

-- This path is changed by the Nix build to point to $out
staticBasePath :: FilePath
staticBasePath = "static/"

getDbFile :: (MonadIO m) => m FilePath
getDbFile = do
  env <- liftIO getEnvironment
  maybe getHomeDbFile pure (lookup "MYOCARDIO_DB_FILE" env)

main :: IO ()
main = do
  homeDbFile' <- getDbFile
  putStrLn $ "using DB file " <> homeDbFile'
  scotty 3000 do
    get "/" do
      DBN.withDatabase \connection -> do
        allMuscles' <- liftIO $ DBN.retrieveAllMuscles connection
        exercises <- liftIO $ DBN.retrieveExercisesWithCurrentIntensity connection DBN.NotCommitted
        currentSoreness <- liftIO $ DBN.retrieveSoreness connection
        html $ renderText $ viewPageCurrentHtml allMuscles' exercises currentSoreness

    get "/exercises" do
      DBN.withDatabase \connection -> do
        exercises <- liftIO $ DBN.retrieveExercisesDescriptions connection
        allMuscles' <- liftIO $ DBN.retrieveAllMuscles connection
        withForm <- queryParamMaybe "with-form"
        editExercise' <- queryParamMaybe "edit-exercise"
        case editExercise' of
          Nothing ->
            html $
              renderText $
                viewExerciseListOuter
                  allMuscles'
                  exercises
                  ( if withForm == Just True
                      then
                        Just
                          DBN.ExerciseDescription
                            { id = 0,
                              muscles = mempty,
                              description = "",
                              name = "",
                              fileIds = mempty
                            }
                      else Nothing
                  )
          Just editExercise'' ->
            case find (\e -> e.name == editExercise'') exercises of
              Nothing -> do
                status status400
                finish
              Just exerciseFound ->
                html $
                  renderText $
                    viewExerciseListOuter allMuscles' exercises (Just exerciseFound)

    get (capture "/training") do
      homeDbFile <- getDbFile
      db <- readDatabase homeDbFile
      html $ renderText $ viewChooseOuter db

    get "/stats" do
      homeDbFile <- getDbFile
      db <- readDatabase homeDbFile
      html $ renderText $ viewStats db

    get (capture "/training/:muscle") do
      homeDbFile <- getDbFile
      db <- readDatabase homeDbFile
      currentTime <- liftIO getCurrentTime
      muscle <- pathParam "muscle"
      html $ renderText $ viewConcreteMuscleGroupExercisesOuter db currentTime muscle

    get "/uploaded-files/:fileid" do
      DBN.withDatabase \connection -> do
        fileId <- pathParam "fileid"
        file <- liftIO $ DBN.retrieveFile connection fileId
        raw file

    get "/remove-exercise/:exercise-name" do
      exerciseName <- pathParam "exercise-name"
      -- Explicit annotation because it could be Text, LazyText, String, ...
      sure :: Maybe Text <- queryParamMaybe "sure"
      if sure == Just "yes"
        then do
          homeDbFile <- getDbFile
          removeExercise homeDbFile exerciseName
          redirect "/"
        else do
          html $ renderText $ viewExerciseDeletion exerciseName

    post "/edit-exercise" do
      -- Very very weird behavior - why is there always at least one file, with not even an empty
      -- file name but ""?
      uploadedFiles <- filter (\(_, fileData) -> fileName fileData /= "\"\"") <$> files
      writtenFiles <- traverse uploadSingleFile uploadedFiles
      musclesRaw <- paramValues exerciseFormMusclesParam
      case traverse (parseParam . TL.fromStrict) musclesRaw of
        Left parseError -> do
          status status400
          text ("I couldn't parse all the muscles you gave me: " <> packShowLazy parseError)
          finish
        Right muscles' -> do
          case NE.nonEmpty muscles' of
            Nothing -> do
              status status400
              text "I got an empty list of muscles!"
              finish
            Just muscles'' -> do
              category' <- formParam exerciseFormCategoryParam
              description' <- formParam exerciseFormDescriptionParam
              name' <- formParam exerciseFormNameParam
              toDelete <- paramValues exerciseFormFilesToDeleteParam
              uploadedFileDir <- getUploadedFileDir
              forM_ toDelete (liftIO . removeFile . unpack . (\fn -> pack uploadedFileDir <> "/" <> fn))
              homeDbFile <- getDbFile
              originalExerciseName <- formParam "original-exercise-name"
              if Text.null originalExerciseName
                then
                  addExercise
                    homeDbFile
                    ( Exercise
                        { muscles = muscles'',
                          category = category',
                          description = description',
                          name = name',
                          fileReferences = writtenFiles
                        }
                    )
                else
                  modifyExercise
                    homeDbFile
                    (ExerciseName originalExerciseName)
                    ( \oldExercise ->
                        let newFiles :: [FileReference]
                            newFiles =
                              filter
                                (\(FileReference fileRef) -> fileRef `notElem` toDelete)
                                oldExercise.fileReferences
                                <> writtenFiles
                         in oldExercise
                              { muscles = muscles'',
                                category = category',
                                description = description',
                                name = name',
                                fileReferences = newFiles
                              }
                    )
              redirect "/exercises"

    post "/toggle-exercise-in-workout" do
      exerciseName <- formParam "exercise-name"
      intensity' <- formParamMaybe "intensity"

      homeDbFile <- getDbFile
      currentTime <- liftIO getCurrentTime
      toggleExercise homeDbFile currentTime exerciseName intensity'
      returnToCurrent :: Maybe Bool <- formParamMaybe "return-to-current"
      redirect case returnToCurrent of
        Nothing ->
          TL.fromStrict "/training"
        Just _ -> "/"

    post "/change-intensity" do
      exerciseName <- formParam "exercise-name"
      intensity' <- formParam "intensity"
      homeDbFile <- getDbFile
      changeIntensity homeDbFile exerciseName intensity'
      redirect "/"

    post "/update-soreness" do
      muscle' <- formParam "muscle"
      howSore' <- formParam "how-sore"

      homeDbFile <- getDbFile
      currentTime <- liftIO getCurrentTime
      addSoreness homeDbFile currentTime muscle' howSore'

      redirect "/"

    post "/reset-soreness" do
      muscle' <- formParam "muscle"
      homeDbFile <- getDbFile
      currentTime <- liftIO getCurrentTime
      addSoreness homeDbFile currentTime muscle' NotSore
      redirect "/"

    post "/commit-workout" do
      homeDbFile <- getDbFile
      commitWorkout homeDbFile
      redirect "/"

    middleware (staticPolicy (noDots <> isNotAbsolute <> addBase staticBasePath))
