{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Applicative (Applicative (pure))
import Control.Lens (Traversal', over, (...))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Crypto.Hash.SHA256 (hashlazy)
import Data.Bifunctor (Bifunctor (second))
import Data.Bool (Bool (True))
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as BSL
import Data.Default (def)
import Data.Either (partitionEithers)
import Data.Eq (Eq ((/=)), (==))
import Data.Foldable (Foldable (foldr, null), find, forM_, minimumBy, notElem)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List (filter, lookup)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Maybe (Maybe (Just, Nothing), mapMaybe, maybe)
import Data.Ord (comparing)
import Data.Semigroup (Semigroup ((<>)))
import Data.String (String)
import Data.Text (Text, isPrefixOf, pack, unpack)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Time (NominalDiffTime)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Data.Tuple (snd)
import Lucid (renderText)
import Myocardio.Database
  ( Category (Strength),
    DatabaseF (pastExercises),
    Exercise (Exercise, category, description, fileReferences, muscles, name),
    ExerciseName (ExerciseName),
    ExerciseWithIntensity (exercise),
    FileReference (FileReference),
    Intensity (Intensity),
    Muscle (Pecs),
    SorenessValue (NotSore),
    addExercise,
    addSoreness,
    changeIntensity,
    commitWorkout,
    exercises,
    getHomeDbFile,
    modifyExercise,
    readDatabase,
    removeExercise,
    toggleExercise,
  )
import Network.HTTP.Types.Status (status400)
import Network.Wai.Middleware.Static (addBase, isNotAbsolute, noDots, staticPolicy)
import Network.Wai.Parse (FileInfo (fileContent, fileName))
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Environment (getEnvironment)
import System.Environment.XDG.BaseDir (getUserDataDir)
import System.IO (FilePath, IO, putStrLn)
import Text.XML (Document, Name, readFile, renderLBS)
import Text.XML.Lens (attributeSatisfies, attrs, named, root)
import Util (packShow, packShowLazy)
import Views (exerciseFormCategoryParam, exerciseFormDescriptionParam, exerciseFormFilesToDeleteParam, exerciseFormMusclesParam, exerciseFormNameParam, musclesAndLastExerciseSorted, viewChooseOuter, viewConcreteMuscleGroupExercisesOuter, viewExerciseDeletion, viewExerciseListOuter, viewPageCurrentHtml, viewStats)
import Web.Scotty (ActionM, File, Parsable (parseParam, parseParamList), capture, file, files, finish, formParam, formParamMaybe, formParams, get, html, middleware, pathParam, post, queryParamMaybe, raw, readEither, redirect, regex, scotty, setHeader, status, text)
import Prelude (Double, Either (Left, Right), Fractional ((/)), Num ((*), (+), (-)), RealFrac (round), Show (show), Traversable (traverse), realToFrac)

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

-- This path is changed by the Nix build to point to $out
svgBasePath :: FilePath
svgBasePath = "svgs/"

data MuscleWithWeight = MuscleWithWeight
  { muscle :: Muscle,
    weight :: Double
  }

fromTuple :: (Muscle, Double) -> MuscleWithWeight
fromTuple (m, w) = MuscleWithWeight {muscle = m, weight = w}

muscleLens :: Muscle -> Traversal' Document (Map.Map Name Text)
muscleLens muscle' = root . named "svg" ... named "path" . attributeSatisfies "id" (isPrefixOf (packShow muscle' <> "-")) . attrs

hsvGreen :: Double
hsvGreen = 120.0

applyWeight :: MuscleWithWeight -> Document -> Document
applyWeight (MuscleWithWeight muscle' weight') =
  over
    (muscleLens muscle')
    (Map.insert "style" ("fill:hsl(" <> pack (show (round @Double @Int (weight' * hsvGreen)) <> ", 50%, 50%)")))

generateWeightedSvg :: (MonadIO m) => [MuscleWithWeight] -> String -> m BSL.ByteString
generateWeightedSvg muscleWeights frontOrBack = do
  oldDocument <- liftIO $ readFile def (svgBasePath <> "/" <> frontOrBack <> ".svg")
  pure
    $ renderLBS
      def
    $ foldr applyWeight oldDocument muscleWeights

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
      homeDbFile <- getDbFile
      db <- readDatabase homeDbFile
      currentTime <- liftIO getCurrentTime
      html $ renderText $ viewPageCurrentHtml currentTime db

    get (regex "/muscles/([^\\.]*).svg") do
      homeDbFile <- getDbFile
      db <- readDatabase homeDbFile
      frontOrBack <- pathParam "1"
      currentTime <- liftIO getCurrentTime
      let lastExercises :: [(Muscle, Maybe UTCTime)]
          lastExercises = musclesAndLastExerciseSorted (filter (\pe -> pe.exercise.category == Strength) db.pastExercises)
          muscleToCategory :: (Muscle, Maybe UTCTime) -> Either Muscle (Muscle, UTCTime)
          muscleToCategory (m, Nothing) = Left m
          muscleToCategory (m, Just t) = Right (m, t)
          (musclesWithoutTime', musclesWithTime') = partitionEithers (muscleToCategory <$> lastExercises)
          musclesWithoutTimeZeroed = (`MuscleWithWeight` 0.0) <$> musclesWithoutTime'
          weightedExercises :: [MuscleWithWeight]
          weightedExercises =
            musclesWithoutTimeZeroed <> case NE.nonEmpty musclesWithTime' of
              Just musclesWithTimeNE ->
                let minTime :: UTCTime
                    minTime = snd $ minimumBy (comparing snd) musclesWithTimeNE
                    -- maxTime :: UTCTime
                    -- maxTime = snd $ maximumBy (comparing snd) musclesWithTimeNE
                    minMaxSpan :: NominalDiffTime
                    minMaxSpan = currentTime `diffUTCTime` minTime
                    lerpMin :: Double
                    lerpMin =
                      if null musclesWithoutTime'
                        then 0.0
                        else 0.2
                    lerpTime :: UTCTime -> Double
                    lerpTime t =
                      realToFrac ((t `diffUTCTime` minTime) / minMaxSpan) * (1 - lerpMin) + lerpMin
                    lerped :: NE.NonEmpty MuscleWithWeight
                    lerped = (fromTuple . second lerpTime <$> musclesWithTimeNE)
                 in NE.toList lerped
              _noMusclesWithTime -> []
      setHeader "Content-Type" "image/svg+xml"
      weightedSvg <- generateWeightedSvg weightedExercises frontOrBack
      raw weightedSvg

    get "/exercises" do
      homeDbFile <- getDbFile
      db <- readDatabase homeDbFile
      withForm <- queryParamMaybe "with-form"
      editExercise' <- queryParamMaybe "edit-exercise"
      case editExercise' of
        Nothing ->
          html $
            renderText $
              viewExerciseListOuter
                db
                ( if withForm == Just True
                    then
                      Just
                        Exercise
                          { muscles = NE.singleton Pecs,
                            category = Strength,
                            description = "",
                            name = ExerciseName "",
                            fileReferences = []
                          }
                    else Nothing
                )
        Just editExercise'' ->
          case find (\e -> e.name == editExercise'') db.exercises of
            Nothing -> do
              status status400
              finish
            Just exerciseFound ->
              html $
                renderText $
                  viewExerciseListOuter db (Just exerciseFound)

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

    get "/uploaded-files/:fn" do
      fileName <- pathParam "fn"
      uploadedFileDir <- getUploadedFileDir
      file (uploadedFileDir <> "/" <> fileName)

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
      toggleExercise homeDbFile exerciseName intensity'
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
      addSoreness homeDbFile muscle' howSore'

      redirect "/"

    post "/reset-soreness" do
      muscle' <- formParam "muscle"
      homeDbFile <- getDbFile
      addSoreness homeDbFile muscle' NotSore
      redirect "/"

    post "/commit-workout" do
      homeDbFile <- getDbFile
      commitWorkout homeDbFile
      redirect "/"

    middleware (staticPolicy (noDots <> isNotAbsolute <> addBase staticBasePath))
