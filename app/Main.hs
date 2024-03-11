{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import CMarkGFM (commonmarkToHtml)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bool (Bool)
import Data.Eq (Eq ((/=)), (==))
import Data.Foldable (Foldable (elem), find, forM_, mapM_)
import Data.Function (($), (.))
import Data.Int (Int)
import Data.List (filter)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (Maybe (Just, Nothing), mapMaybe, maybe)
import Data.Monoid (Monoid (mempty))
import Data.Ord (comparing)
import Data.Semigroup (Semigroup ((<>)))
import Data.String (IsString)
import Data.Text (Text, pack)
import qualified Data.Text.Lazy as TL
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime, nominalDay)
import Lucid (renderText)
import qualified Lucid as L
import MyocardioApp.Database
  ( Category (Strength),
    Database,
    DatabaseF (currentTraining, pastExercises, sorenessHistory),
    Exercise (Exercise, category, description, muscles, name),
    ExerciseName (ExerciseName),
    ExerciseWithIntensity (ExerciseWithIntensity, exercise, intensity, time),
    Intensity (Intensity),
    Muscle,
    Soreness (Soreness, muscle, soreness, time),
    SorenessValue (LittleSore, NotSore, VerySore),
    allCategories,
    allMuscles,
    exercises,
    intensityToText,
    modifyDb,
    readDatabase,
  )
import qualified MyocardioApp.Htmx as LX
import Network.HTTP.Types.Status (status400)
import Safe (maximumByMay)
import System.IO (IO)
import Web.Scotty (Parsable (parseParam, parseParamList), finish, get, html, param, post, readEither, scotty, status)
import Prelude (Either (Left, Right), Fractional ((/)), RealFrac (round), Show (show))

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

newtype HtmlId = HtmlId Text

makeId :: HtmlId -> L.Attributes
makeId (HtmlId i) = L.id_ i

makeTarget :: HtmlId -> L.Attributes
makeTarget (HtmlId i) = LX.hxTarget_ ("#" <> i)

packShow :: (Show a) => a -> Text
packShow = pack . show

idTopLevelContainer :: (IsString a) => a
idTopLevelContainer = "top-level-container"

htmlSkeleton :: CurrentPage -> L.Html a -> L.Html a
htmlSkeleton page content = do
  L.doctypehtml_ $ do
    L.head_ $ do
      LX.useHtmx
      L.meta_ [L.charset_ "utf-8"]
      L.meta_ [L.name_ "viewport", L.content_ "width=device-width, initial-scale=1"]
      L.title_ "myocardio - power up your routines"
      L.link_ [L.href_ "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css", L.rel_ "stylesheet"]
      L.link_ [L.href_ "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css", L.rel_ "stylesheet"]
    L.body_ $ do
      headerHtml page
      L.div_ [L.class_ "container", L.id_ idTopLevelContainer] $ do
        content

sorenessValueToEmoji :: SorenessValue -> Text
sorenessValueToEmoji VerySore = "😭"
sorenessValueToEmoji LittleSore = "😕"
sorenessValueToEmoji _otherSoreness = ""

sorenessOutput :: Database -> L.Html ()
sorenessOutput database = do
  let muscleToSoreness :: Muscle -> Maybe Soreness
      muscleToSoreness muscle' =
        case maximumByMay (comparing (.time)) $ filter (\historyEntry -> historyEntry.muscle == muscle') database.sorenessHistory of
          -- If the latest value is not sore, then don't display soreness at all.
          Just (Soreness {soreness = NotSore}) -> Nothing
          otherValue -> otherValue
      sorenessToHtml :: Soreness -> L.Html ()
      sorenessToHtml soreness' = L.li_ do
        L.span_ [L.class_ "me-1"] (L.toHtml (sorenessValueToEmoji soreness'.soreness))
        L.strong_ [L.class_ "me-1"] (L.toHtml (packShow soreness'.muscle))
        L.a_
          [ LX.hxPost_ ("/reset-soreness?muscle=" <> packShow soreness'.muscle),
            L.href_ "#",
            LX.hxTarget_ ("#" <> anchorSorenessOutput)
          ]
          "Reset"
  L.div_ [L.id_ "soreness-output"] do
    L.ul_ (mapM_ sorenessToHtml $ mapMaybe muscleToSoreness allMuscles)

urlNewSoreness :: (IsString a) => a
urlNewSoreness = "/new-soreness"

urlNewExercise :: (IsString a) => a
urlNewExercise = "/partial/new-exercise"

urlAddToWorkout :: (IsString a) => a
urlAddToWorkout = "/add-to-workout"

addToWorkoutExerciseName :: (IsString a) => a
addToWorkoutExerciseName = "exercise-name"

addToWorkoutIntensity :: (IsString a) => a
addToWorkoutIntensity = "intensity"

newSorenessMuscle :: (IsString a) => a
newSorenessMuscle = "muscle"

newSorenessHowSore :: (IsString a) => a
newSorenessHowSore = "how-sore"

anchorSorenessOutput :: (IsString a) => a
anchorSorenessOutput = "soreness-output"

anchorCurrentWorkout :: (IsString a) => a
anchorCurrentWorkout = "current-workout"

icon :: Text -> L.Html ()
icon name' = L.i_ [L.class_ ("bi-" <> name' <> " me-2")] mempty

sorenessInputAndOutput :: Database -> L.Html ()
sorenessInputAndOutput database = do
  L.h1_ do
    icon "graph-down-arrow"
    L.span_ "Soreness"
  L.form_ [L.class_ "row"] do
    L.div_ [L.class_ "col"] do
      L.div_ [L.class_ "form-floating"] do
        let muscleOption :: Muscle -> L.Html ()
            muscleOption muscle' = L.option_ [L.value_ (packShow muscle')] (L.toHtml (packShow muscle'))
        L.select_ [L.class_ "form-select", L.id_ "i-am-sore", L.name_ newSorenessMuscle] (mapM_ muscleOption allMuscles)
        L.label_ [L.for_ "i-am-sore"] "I am sore here"
    L.div_ [L.class_ "col"] do
      L.div_ [L.class_ "form-check form-check-inline"] do
        L.input_ [L.class_ "form-check-input", L.type_ "radio", L.name_ newSorenessHowSore, L.id_ "very sore", L.value_ (packShow VerySore), L.checked_]
        L.label_ [L.class_ "form-check-label", L.for_ "very sore"] (L.toHtml $ sorenessValueToEmoji VerySore <> " VERY")
      L.div_ [L.class_ "form-check form-check-inline"] do
        L.input_ [L.class_ "form-check-input", L.type_ "radio", L.name_ newSorenessHowSore, L.id_ "a little", L.value_ (packShow LittleSore)]
        L.label_ [L.class_ "form-check-label", L.for_ "a little"] (L.toHtml $ sorenessValueToEmoji LittleSore <> " A LITTLE")
    L.div_ [L.class_ "col"] do
      L.button_
        [ L.class_ "btn btn-primary",
          L.type_ "submit",
          LX.hxPost_ urlNewSoreness,
          LX.hxTarget_ ("#" <> anchorSorenessOutput)
        ]
        "Submit"
  sorenessOutput database

dayDiffText :: UTCTime -> UTCTime -> Text
dayDiffText currentTime before =
  let dayDiff = round $ diffUTCTime currentTime before / nominalDay
   in case dayDiff :: Int of
        0 -> "today"
        1 -> "yesterday"
        n -> packShow n <> " days ago"

currentWorkoutHtml :: Database -> L.Html ()
currentWorkoutHtml database =
  L.div_ [L.id_ anchorCurrentWorkout] do
    case database.currentTraining of
      [] -> mempty
      currentExercises -> do
        L.h1_ do
          icon "joystick"
          L.span_ "Current Workout"
        L.ol_ $ forM_ currentExercises \exWithIn -> do
          L.li_ do
            L.span_ do
              L.a_ [L.href_ ("#description-" <> packShow exWithIn.exercise.name)] (L.strong_ (L.toHtml (packShow exWithIn.exercise.name)))
            L.span_ ", "
            L.span_ [L.class_ "me-2"] (L.toHtml (intensityToText exWithIn.intensity))
            L.a_
              [ LX.hxPost_ ("/reset-current-workout?exercise-name=" <> packShow exWithIn.exercise.name),
                L.href_ "#",
                LX.hxTarget_ ("#" <> anchorCurrentWorkout)
              ]
              "Reset"

trainingHtml :: UTCTime -> Database -> L.Html ()
trainingHtml currentTime database = do
  L.h1_ do
    icon "hand-thumbs-up"
    L.span_ "Training"
  let exerciseWithIntensityTrainsMuscle :: Muscle -> ExerciseWithIntensity Exercise -> Bool
      exerciseWithIntensityTrainsMuscle muscle' e = muscle' `elem` e.exercise.muscles
      lastTraining :: Muscle -> L.Html ()
      lastTraining muscle' =
        case maximumByMay
          (comparing (.time))
          (filter (exerciseWithIntensityTrainsMuscle muscle') database.pastExercises) of
          Nothing -> L.p_ do
            L.strong_ "never trained!"
          Just exWithIntensity ->
            L.p_
              [L.class_ "text-muted"]
              ( L.toHtml $
                  "Last training: "
                    <> dayDiffText currentTime exWithIntensity.time
                    <> " "
                    <> packShow exWithIntensity.exercise.name
              )
      exercisesByCategory :: Muscle -> [NE.NonEmpty Exercise]
      exercisesByCategory muscle' = NE.groupAllWith (.category) $ filter (\e -> muscle' `elem` e.muscles) database.exercises
      outputExercise :: Exercise -> L.Html ()
      outputExercise e = do
        let pastExercise = maximumByMay (comparing (.time)) $ filter (\pe -> pe.exercise.name == e.name) database.pastExercises
        L.form_ do
          L.input_
            [ L.type_ "hidden",
              L.name_ addToWorkoutExerciseName,
              L.value_ (packShow e.name)
            ]
          L.div_ [L.class_ "input-group"] do
            L.button_
              [ L.type_ "button",
                LX.hxPost_ urlAddToWorkout,
                L.class_ "btn btn-sm btn-primary",
                LX.hxTarget_ ("#" <> anchorCurrentWorkout)
              ]
              do
                icon "journal-plus"
                L.span_ "Add to workout"
            L.input_
              [ L.class_ "form-control",
                L.value_ (maybe "" (intensityToText . (.intensity)) pastExercise),
                L.name_ addToWorkoutIntensity,
                L.type_ "text"
              ]
            L.span_ [L.class_ "input-group-text"] (L.toHtml $ packShow e.name)
            L.span_ [L.class_ "text-muted input-group-text"] $ L.toHtml $ case pastExercise of
              Nothing -> "never executed!"
              Just lastExecution -> dayDiffText currentTime lastExecution.time
      muscleToTrainingHtml :: Muscle -> L.Html ()
      muscleToTrainingHtml muscle' = do
        L.h2_ (L.toHtml $ packShow muscle')
        lastTraining muscle'
        forM_ (exercisesByCategory muscle') \exercises' -> do
          L.h3_ (L.toHtml $ packShow $ (.category) $ NE.head exercises')
          forM_ exercises' outputExercise

  mapM_ muscleToTrainingHtml allMuscles

urlNewExerciseCancel :: (IsString a) => a
urlNewExerciseCancel = "/partial/new-exercise-cancel"

urlNewExerciseSubmit :: (IsString a) => a
urlNewExerciseSubmit = "/partial/new-exercise-submit"

exerciseFormMusclesParam :: (IsString a) => a
exerciseFormMusclesParam = "muscles"

exerciseFormCategoryParam :: (IsString a) => a
exerciseFormCategoryParam = "category"

exerciseFormNameParam :: (IsString a) => a
exerciseFormNameParam = "name"

exerciseFormDescriptionParam :: (IsString a) => a
exerciseFormDescriptionParam = "description"

idExerciseForm :: HtmlId
idExerciseForm = HtmlId "new-exercise-form"

newExerciseFormHtml :: L.Html ()
newExerciseFormHtml = do
  L.form_ do
    L.div_ [L.class_ "form-floating mb-3"] do
      L.input_ [L.class_ "form-control", L.id_ "exercise-name", L.type_ "text", L.name_ exerciseFormNameParam]
      L.label_ [L.for_ "exercise-name"] "Name"

    L.h5_ "Category"
    L.div_ [L.class_ "d-flex justify-content-evenly mb-3"] $ forM_ allCategories \category' -> do
      L.input_
        [ L.class_ "btn-check",
          L.id_ ("category-" <> packShow category'),
          L.type_ "radio",
          L.name_ exerciseFormCategoryParam,
          if category' == Strength then L.checked_ else mempty,
          L.value_ (packShow category')
        ]
      L.label_
        [L.for_ ("category-" <> packShow category'), L.class_ "btn btn-outline-info"]
        $ L.toHtml
        $ packShow category'

    L.h5_ "Muscles involved"
    L.div_ [L.class_ "mb-3"] $ forM_ allMuscles \muscle' -> do
      L.input_
        [ L.class_ "btn-check",
          L.id_ ("muscle-" <> packShow muscle'),
          L.type_ "checkbox",
          L.name_ exerciseFormMusclesParam,
          L.value_ (packShow muscle')
        ]
      L.label_
        [L.for_ ("muscle-" <> packShow muscle'), L.class_ "btn btn-outline-secondary me-2"]
        $ L.toHtml
        $ packShow muscle'

    L.textarea_ [L.class_ "form-control mb-3", L.placeholder_ "Description", L.name_ exerciseFormDescriptionParam] mempty

    L.div_ [L.class_ "hstack gap-3"] do
      L.button_
        [ L.type_ "submit",
          L.class_ "btn btn-outline-primary",
          LX.hxPost_ urlNewExerciseSubmit,
          LX.hxTarget_ ("#" <> idTopLevelContainer)
        ]
        "Submit"
      L.button_
        [ L.type_ "submit",
          L.class_ "btn btn-outline-warning",
          LX.hxPost_ urlNewExerciseCancel,
          makeTarget idExerciseForm
        ]
        "Cancel edit"

newExerciseButtonHtml :: L.Html ()
newExerciseButtonHtml =
  L.button_
    [ L.type_ "submit",
      L.class_ "btn btn-primary",
      LX.hxPost_ urlNewExercise,
      makeTarget idExerciseForm
    ]
    do
      icon "plus-lg"
      L.span_ "New exercise"

exercisesHtml :: Database -> L.Html ()
exercisesHtml db = do
  L.div_ [makeId idExerciseForm, L.class_ "mb-3"] newExerciseButtonHtml
  L.hr_ []
  L.h1_ do
    icon "box2-heart"
    L.span_ "Exercise Descriptions"
  forM_ db.exercises \exercise' -> do
    L.h4_ [L.id_ ("description-" <> packShow exercise'.name)] (L.toHtml (packShow exercise'.name))
    L.toHtmlRaw $ commonmarkToHtml [] [] exercise'.description

data CurrentPage = PageSoreness | PageTraining | PageExercises deriving (Eq)

headerHtml :: CurrentPage -> L.Html ()
headerHtml currentPage =
  let pages =
        [ (PageSoreness, "", "graph-down-arrow", "Soreness"),
          (PageTraining, "training", "joystick", "Training"),
          (PageExercises, "exercises", "box2-heart", "Exercises")
        ]
      makeItem :: (CurrentPage, Text, Text, Text) -> L.Html ()
      makeItem (enum, url, iconName, title) =
        L.li_ [L.class_ "nav-item"] do
          L.a_
            [ L.href_ ("/" <> url),
              L.class_ "nav-link",
              if currentPage == enum then L.class_ "active" else mempty
            ]
            do
              icon iconName
              L.toHtml title
   in L.header_ [L.class_ "d-flex justify-content-center py-3"] do
        L.ul_ [L.class_ "nav nav-pills"] (mapM_ makeItem pages)

main :: IO ()
main = scotty 3000 do
  post urlNewSoreness do
    muscle' <- param newSorenessMuscle
    howSore' <- param newSorenessHowSore

    currentTime <- liftIO getCurrentTime

    db <-
      modifyDb
        ( \db ->
            db
              { sorenessHistory =
                  Soreness
                    { time = currentTime,
                      muscle = muscle',
                      soreness = howSore'
                    }
                    : db.sorenessHistory
              }
        )

    html $ renderText $ sorenessOutput db
  post "/reset-soreness" do
    muscle' <- param "muscle"

    currentTime <- liftIO getCurrentTime

    db <-
      modifyDb
        ( \db ->
            db
              { sorenessHistory =
                  Soreness
                    { time = currentTime,
                      muscle = muscle',
                      soreness = NotSore
                    }
                    : db.sorenessHistory
              }
        )

    html $ renderText $ sorenessOutput db
  post urlNewExerciseSubmit do
    muscles' <- param exerciseFormMusclesParam
    category' <- param exerciseFormCategoryParam
    description' <- param exerciseFormDescriptionParam
    name' <- param exerciseFormNameParam
    newDb <- modifyDb \db ->
      db {exercises = Exercise {muscles = muscles', category = category', description = description', name = name'} : db.exercises}
    html $ renderText $ exercisesHtml newDb
  post urlNewExerciseCancel do
    html $ renderText newExerciseButtonHtml
  post urlNewExercise do
    html $ renderText newExerciseFormHtml
  post urlAddToWorkout do
    exerciseName :: ExerciseName <- param addToWorkoutExerciseName
    intensity' :: Intensity <- param addToWorkoutIntensity
    currentTime <- liftIO getCurrentTime
    readDb <- readDatabase
    case find (\e -> e.name == exerciseName) readDb.exercises of
      Nothing -> do
        status status400
        finish
      Just exercise' -> do
        db' <-
          modifyDb
            ( \db ->
                db
                  { currentTraining =
                      ExerciseWithIntensity
                        { exercise = exercise',
                          intensity = intensity',
                          time = currentTime
                        }
                        : db.currentTraining
                  }
            )
        html $ renderText $ currentWorkoutHtml db'

  post "/reset-current-workout" do
    exercise' :: ExerciseName <- param "exercise-name"
    db <-
      modifyDb
        ( \db ->
            db
              { currentTraining =
                  filter (\exWithIn -> exWithIn.exercise.name /= exercise') db.currentTraining
              }
        )
    html $ renderText $ currentWorkoutHtml db

  get "/exercises" do
    db <- readDatabase
    html $ renderText $ htmlSkeleton PageExercises $ exercisesHtml db

  get "/training" do
    db <- readDatabase
    currentTime <- liftIO getCurrentTime
    html $ renderText $ htmlSkeleton PageTraining $ do
      currentWorkoutHtml db
      L.hr_ [L.class_ "mb-3"]
      trainingHtml currentTime db

  get "/" do
    db <- readDatabase
    html $ renderText $ htmlSkeleton PageSoreness $ sorenessInputAndOutput db
