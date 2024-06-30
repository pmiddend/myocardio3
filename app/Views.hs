{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Views
  ( viewStats,
    viewPageCurrentHtml,
    musclesAndLastExerciseSorted,
    viewExerciseListOuter,
    viewChooseOuter,
    viewConcreteMuscleGroupExercisesOuter,
    viewExerciseDeletion,
    exerciseFormMusclesParam,
    exerciseFormFilesToDeleteParam,
    exerciseFormCategoryParam,
    exerciseFormDescriptionParam,
    exerciseFormNameParam,
  )
where

import CMarkGFM (commonmarkToHtml)
import Control.Monad (unless, when, (>>=))
import Data.Bool (Bool (True), (&&))
import Data.Eq (Eq, (==))
import Data.Foldable (Foldable (elem, length), find, forM_, mapM_)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List (filter, reverse, sortBy, zip)
import Data.List.NonEmpty qualified as NE
import Data.List.Split (chunksOf)
import Data.Maybe (Maybe (Just, Nothing), fromMaybe, isJust, isNothing, mapMaybe, maybe)
import Data.Monoid (Monoid (mempty))
import Data.Ord (Ord ((<), (<=), (>)), comparing)
import Data.Semigroup (Semigroup ((<>)))
import Data.Set qualified as Set
import Data.String (IsString)
import Data.Text (Text, pack, replace)
import Data.Time (Day (ModifiedJulianDay))
import Data.Time.Clock (UTCTime (utctDay, utctDayTime), diffUTCTime, nominalDay)
import Lucid qualified as L
import Lucid.Base (makeAttributes)
import Myocardio.Database
  ( Category (Strength),
    Database,
    DatabaseF (currentTraining, pastExercises, sorenessHistory),
    Exercise (Exercise, category, description, fileReferences, muscles, name),
    ExerciseName,
    ExerciseWithIntensity (exercise, intensity, time),
    FileReference (FileReference),
    Muscle,
    Soreness (Soreness, muscle, soreness, time),
    SorenessValue (LittleSore, NotSore, VerySore),
    allCategories,
    allMuscles,
    exercises,
    intensityToText,
  )
import Safe (maximumByMay)
import Text.Read (Read)
import Util (packShow)
import Web.Scotty (Parsable (parseParam), readEither)
import Prelude (Enum (succ), Fractional ((/)), RealFrac (round), Show)

iconHtml :: Text -> L.Html ()
iconHtml name' = L.i_ [L.class_ ("bi-" <> name' <> " me-2")] mempty

iconHtml' :: Text -> L.Html ()
iconHtml' name' = L.i_ [L.class_ ("bi-" <> name')] mempty

data CurrentPage
  = PageCurrent
  | PageStats
  | PageMuscle Muscle
  | PageExerciseDeletion ExerciseName
  | PageChooseExercise
  | PageExercises
  deriving (Eq, Show, Read)

pageToPath :: CurrentPage -> Text
pageToPath PageCurrent = "/"
pageToPath PageStats = "/stats"
pageToPath (PageExerciseDeletion name) = "/remove-exercise/" <> packShow name
pageToPath PageExercises = "/exercises"
pageToPath PageChooseExercise = "/training"
pageToPath (PageMuscle m) = "/training/" <> packShow m

instance Parsable CurrentPage where
  parseParam = readEither

data PageDescription = PageDescription
  { enum :: !CurrentPage,
    icon :: !Text,
    description :: !Text
  }

viewHeader :: CurrentPage -> L.Html ()
viewHeader currentPage =
  let allPages =
        [ PageDescription PageCurrent "basket" "Current",
          PageDescription PageStats "graph-up-arrow" "Stats",
          PageDescription PageChooseExercise "card-checklist" "Choose",
          PageDescription PageExercises "box2-heart" "Edit"
        ]
      viewItem :: PageDescription -> L.Html ()
      viewItem pd =
        L.li_ [L.class_ "nav-item"] do
          L.a_
            [ L.href_ (pageToPath pd.enum),
              L.class_ "nav-link",
              if currentPage == pd.enum then L.class_ "active" else mempty
            ]
            do
              iconHtml pd.icon
              L.toHtml pd.description
   in L.header_ [L.class_ "py-3"] do
        L.div_ [L.class_ "d-flex justify-content-center align-items-center"] do
          L.ul_
            [L.class_ "nav nav-tabs"]
            (mapM_ viewItem allPages)

viewStats :: Database -> L.Html ()
viewStats _ = viewHtmlSkeleton PageStats mempty

viewHtmlSkeleton :: CurrentPage -> L.Html () -> L.Html ()
viewHtmlSkeleton page content = do
  L.doctypehtml_ $ do
    L.head_ $ do
      L.meta_ [L.charset_ "utf-8"]
      L.meta_ [L.name_ "viewport", L.content_ "width=device-width, initial-scale=1"]
      L.link_ [L.rel_ "apple-touch-icon", L.sizes_ "180x180", L.href_ "/apple-touch-icon.png"]
      L.link_ [L.rel_ "icon", L.type_ "image/png", L.sizes_ "32x32", L.href_ "/favicon-32x32.png"]
      L.link_ [L.rel_ "icon", L.type_ "image/png", L.sizes_ "16x16", L.href_ "/favicon-16x16.png"]
      L.link_ [L.rel_ "manifest", L.href_ "/site.webmanifest"]
      L.title_ "myocardio - power up your routines"
      L.link_ [L.href_ "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css", L.rel_ "stylesheet"]
      L.link_ [L.href_ "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css", L.rel_ "stylesheet"]
    L.body_ $ do
      viewHeader page
      L.div_ [L.class_ "container"] do
        L.main_ do
          content
      -- Not sure if we need the bootstrap JS, and it must save some bandwidth, so leave it out maybe
      L.script_ [L.src_ "https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js"] ("" :: Text)

newtype HtmlId = HtmlId Text

htmlIdFromText :: Text -> Text
htmlIdFromText = replace " " "_"

makeId :: HtmlId -> L.Attributes
makeId (HtmlId i) = L.id_ i

makeHref :: HtmlId -> L.Attributes
makeHref (HtmlId i) = L.href_ ("#" <> i)

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
      sorenessToHtml soreness' = L.li_ $ L.form_ [L.action_ "/reset-soreness", L.method_ "post"] do
        L.input_ [L.type_ "hidden", L.name_ "muscle", L.value_ (packShow soreness'.muscle)]
        L.span_ [L.class_ "me-1"] (L.toHtml (sorenessValueToEmoji soreness'.soreness))
        L.strong_ [L.class_ "me-1"] (L.toHtml (packShow soreness'.muscle))
        L.button_
          [L.type_ "submit", L.class_ "btn btn-link"]
          "Reset"
  L.div_ [L.id_ "soreness-output"] do
    L.ul_ (mapM_ sorenessToHtml $ mapMaybe muscleToSoreness allMuscles)

idCurrentWorkout :: HtmlId
idCurrentWorkout = HtmlId "current-workout"

idSoreness :: HtmlId
idSoreness = HtmlId "soreness"

idExerciseHistory :: HtmlId
idExerciseHistory = HtmlId "exercise-history"

sorenessInputAndOutput :: Database -> L.Html ()
sorenessInputAndOutput database = do
  L.h1_ [makeId idSoreness] do
    iconHtml "graph-down-arrow"
    L.span_ "Soreness"
  L.form_ [L.action_ "/update-soreness", L.method_ "post"] do
    L.div_ [L.class_ "mb-2"] do
      L.div_ [L.class_ "form-floating"] do
        let muscleOption :: Muscle -> L.Html ()
            muscleOption muscle' = L.option_ [L.value_ (packShow muscle')] (L.toHtml (packShow muscle'))
        L.select_ [L.class_ "form-select", L.id_ "i-am-sore", L.name_ "muscle"] (mapM_ muscleOption allMuscles)
        L.label_ [L.for_ "i-am-sore"] "I am sore here"
    L.div_ [L.class_ "d-flex justify-content-evenly align-items-center mb-2"] do
      L.input_ [L.class_ "btn-check", L.type_ "radio", L.name_ "how-sore", L.id_ "verysore", L.value_ (packShow VerySore), L.checked_]
      L.label_ [L.class_ "btn", L.for_ "verysore"] (L.toHtml $ sorenessValueToEmoji VerySore <> " VERY")
      L.input_ [L.class_ "btn-check", L.type_ "radio", L.name_ "how-sore", L.id_ "alittle", L.value_ (packShow LittleSore)]
      L.label_ [L.class_ "btn", L.for_ "alittle"] (L.toHtml $ sorenessValueToEmoji LittleSore <> " A LITTLE")
    L.div_ [L.class_ "d-flex justify-content-center"] do
      L.button_
        [ L.class_ "btn btn-primary",
          L.type_ "submit"
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

viewExerciseImageCarousel :: Exercise -> L.Html ()
viewExerciseImageCarousel exercise = do
  L.div_
    [ L.class_ "carousel slide",
      L.id_ ("carousel-" <> htmlIdFromText (packShow exercise.name)),
      makeAttributes "data-bs-theme" "dark"
    ]
    $ do
      L.div_
        [L.class_ "carousel-inner"]
        ( forM_ (zip [0 ..] exercise.fileReferences) \(i, FileReference fileRef) ->
            L.div_
              [ L.class_ "carousel-item" <> if i == (0 :: Int) then L.class_ "active" else mempty
              ]
              $ L.img_ [L.src_ ("/uploaded-files/" <> fileRef), L.class_ "d-block w-100"]
        )
      unless (length exercise.fileReferences <= 1) do
        L.button_
          [ L.type_ "button",
            L.class_ "carousel-control-prev",
            makeAttributes "data-bs-target" ("#carousel-" <> htmlIdFromText (packShow exercise.name)),
            makeAttributes "data-bs-slide" "prev"
          ]
          (L.span_ [L.class_ "carousel-control-prev-icon"] mempty)
        L.button_
          [ L.type_ "button",
            L.class_ "carousel-control-next",
            makeAttributes "data-bs-target" ("#carousel-" <> htmlIdFromText (packShow exercise.name)),
            makeAttributes "data-bs-slide" "next"
          ]
          (L.span_ [L.class_ "carousel-control-next-icon"] mempty)

viewCurrentWorkout :: Database -> L.Html ()
viewCurrentWorkout database =
  L.div_ [makeId idCurrentWorkout] do
    case database.currentTraining of
      [] -> mempty
      currentExercises -> do
        L.h1_ do
          iconHtml "joystick"
          L.span_ "Current"
        let musclesInvolved = currentExercises >>= (NE.toList . (.muscles) . (.exercise))
            musclesMissing = Set.toList $ Set.fromList allMuscles `Set.difference` Set.fromList musclesInvolved
        L.div_ [L.class_ "gap-1 mb-3"] do
          L.span_ [L.class_ "text-muted me-1"] "Trained: "
          forM_ musclesInvolved \muscle' -> L.span_ [L.class_ "badge text-bg-success me-1"] (L.toHtml $ packShow muscle')
        L.div_ [L.class_ "gap-1 mb-3"] do
          L.span_ [L.class_ "text-muted me-1"] "Missing: "
          forM_ musclesMissing \muscle' -> L.span_ [L.class_ "badge text-bg-warning me-1"] (L.toHtml $ packShow muscle')
        forM_ currentExercises \exWithIn -> do
          L.div_ [L.class_ "mb-3 card"] do
            viewExerciseImageCarousel exWithIn.exercise
            L.div_ [L.class_ "card-body"] do
              L.h5_ [L.class_ "card-title"] do
                L.strong_ (L.toHtml (packShow exWithIn.exercise.name))
              L.h6_ [L.class_ "card-subtitle"] do
                "Intensity: " <> L.strong_ (L.toHtml (intensityToText exWithIn.intensity))
              L.p_ [L.class_ "card-text text-info"] do
                L.toHtmlRaw $ commonmarkToHtml [] [] exWithIn.exercise.description
                L.form_ [L.action_ "/toggle-exercise-in-workout", L.method_ "post"] do
                  L.input_ [L.type_ "hidden", L.name_ "return-to-current", L.value_ (packShow True)]
                  L.input_ [L.type_ "hidden", L.name_ "exercise-name", L.value_ (packShow exWithIn.exercise.name)]
                  L.button_
                    [ L.type_ "submit",
                      L.class_ "btn btn-secondary btn-sm"
                    ]
                    do
                      iconHtml "trash"
                      L.span_ "Remove from workout"
                L.form_ [L.action_ "/change-intensity", L.method_ "post"] do
                  L.input_ [L.type_ "hidden", L.name_ "exercise-name", L.value_ (packShow exWithIn.exercise.name)]
                  L.input_
                    [ L.class_ "form-control mb-1",
                      L.value_ (intensityToText exWithIn.intensity),
                      L.name_ "intensity",
                      L.type_ "text"
                    ]
                  L.button_
                    [ L.type_ "submit",
                      L.class_ "btn btn-sm btn-primary"
                    ]
                    do
                      iconHtml "send"
                      L.span_ "Change intensity"

        L.form_ [L.action_ "/commit-workout", L.method_ "post"] do
          L.button_
            [ L.type_ "submit",
              L.class_ "btn btn-primary"
            ]
            do
              iconHtml "send"
              L.span_ "Commit workout"

inCurrentTraining :: Database -> Muscle -> Bool
inCurrentTraining db muscle =
  isJust (find (\ewi -> muscle `elem` ewi.exercise.muscles) db.currentTraining)

viewSingleExerciseInChooser :: UTCTime -> Database -> Muscle -> Exercise -> L.Html ()
viewSingleExerciseInChooser currentTime database muscle' exercise =
  let lastExecutionOfThisExercise :: Maybe (ExerciseWithIntensity Exercise)
      lastExecutionOfThisExercise =
        maximumByMay (comparing (.time)) $ filter (\pe -> pe.exercise.name == exercise.name) database.pastExercises
      beginningOfDayAfterExecution :: Maybe UTCTime
      beginningOfDayAfterExecution =
        (\x -> x.time {utctDay = succ x.time.utctDay, utctDayTime = 1}) <$> lastExecutionOfThisExercise
      nextExerciseAfterThisContainingThisMuscle :: Maybe (ExerciseWithIntensity Exercise)
      nextExerciseAfterThisContainingThisMuscle =
        beginningOfDayAfterExecution >>= \x -> find (\e -> e.time > x && muscle' `elem` e.exercise.muscles) database.pastExercises
      partOfCurrentWorkout :: Bool
      partOfCurrentWorkout = isJust (find (\e -> e.exercise.name == exercise.name) database.currentTraining)
      pastTimeReadable = case lastExecutionOfThisExercise of
        Nothing -> L.p_ "Never executed!"
        Just lastExecutionInstance ->
          let firstSorenessBetweenExecutions :: Maybe Soreness
              firstSorenessBetweenExecutions =
                find
                  ( \soreness' ->
                      soreness'.time
                        > lastExecutionInstance.time
                        && soreness'.muscle
                          == muscle'
                        && case nextExerciseAfterThisContainingThisMuscle of
                          Nothing -> True
                          Just nextExercise ->
                            soreness'.time < nextExercise.time
                  )
                  database.sorenessHistory
           in do
                L.span_ $ L.toHtml $ "Last: " <> dayDiffText currentTime lastExecutionInstance.time
                L.br_ []
                case firstSorenessBetweenExecutions of
                  Nothing -> L.span_ "No soreness."
                  Just lastSoreness -> L.span_ $ L.toHtml $ "Soreness: " <> sorenessValueToEmoji lastSoreness.soreness
   in L.form_ [L.method_ "post", L.action_ "/toggle-exercise-in-workout"] do
        L.input_
          [ L.type_ "hidden",
            L.name_ "exercise-name",
            L.value_ (packShow exercise.name)
          ]
        L.div_ [L.class_ "mb-3 card"] do
          viewExerciseImageCarousel exercise
          L.div_ [L.class_ "card-body"] do
            L.h5_ [L.class_ "card-title"] do
              L.span_ $ L.toHtml $ packShow exercise.name
              when (isJust lastExecutionOfThisExercise && isNothing nextExerciseAfterThisContainingThisMuscle) do
                L.span_ [L.class_ "ms-2 badge text-bg-secondary"] "Last"
            L.div_ [L.class_ "card-text"] do
              if partOfCurrentWorkout
                then do
                  L.p_ do
                    L.em_ "in current workout"
                  L.button_
                    [ L.class_ "btn btn-secondary btn-sm",
                      L.type_ "submit"
                    ]
                    do
                      iconHtml "trash"
                      "Remove from workout"
                else do
                  L.div_ [L.class_ "text-muted mb-3"] pastTimeReadable
                  L.div_ [L.class_ "mb-3"] do
                    L.input_
                      [ L.class_ "form-control mb-1",
                        L.value_ (maybe "" (intensityToText . (.intensity)) lastExecutionOfThisExercise),
                        L.name_ "intensity",
                        L.type_ "text",
                        L.placeholder_ "Intensity?"
                      ]
                    L.button_
                      [ L.type_ "submit",
                        L.class_ "btn btn-sm btn-primary"
                      ]
                      do
                        iconHtml "journal-plus"
                        L.span_ "Add to workout"

viewConcreteMuscleGroupExercisesOuter :: Database -> UTCTime -> Muscle -> L.Html ()
viewConcreteMuscleGroupExercisesOuter db currentTime muscle = viewHtmlSkeleton (PageMuscle muscle) (viewConcreteMuscleGroupExercises currentTime db muscle)

viewConcreteMuscleGroupExercises :: UTCTime -> Database -> Muscle -> L.Html ()
viewConcreteMuscleGroupExercises currentTime database muscle = do
  let exercisePool = filter (\e -> muscle `elem` e.muscles) database.exercises
      exerciseWithIntensityTrainsMuscle :: Muscle -> ExerciseWithIntensity Exercise -> Bool
      exerciseWithIntensityTrainsMuscle muscle' e = muscle' `elem` e.exercise.muscles
      lastTraining :: Muscle -> L.Html ()
      lastTraining muscle' =
        case maximumByMay
          (comparing (.time))
          (filter (exerciseWithIntensityTrainsMuscle muscle') database.pastExercises) of
          Nothing -> L.p_ do
            L.em_ "never trained!"
          Just exWithIntensity ->
            L.p_ [L.class_ "text-muted"] do
              L.em_ $ do
                L.toHtml $ "Last training: " <> dayDiffText currentTime exWithIntensity.time <> ": "
                L.strong_ $ L.toHtml $ packShow exWithIntensity.exercise.name
      exercisesForMuscle :: Muscle -> [Exercise]
      exercisesForMuscle muscle' = filter (\e -> muscle' `elem` e.muscles) exercisePool

  case exercisesForMuscle muscle of
    [] -> mempty
    exercisesForThisMuscle -> do
      L.h2_ [L.id_ ("training-section-" <> htmlIdFromText (packShow muscle)), L.class_ "mt-3"] (L.toHtml $ packShow muscle)
      L.div_ do
        lastTraining muscle
        L.div_ do
          forM_ exercisesForThisMuscle (viewSingleExerciseInChooser currentTime database muscle)

viewChoose :: Database -> L.Html ()
viewChoose database = do
  let viewButtonClass muscle' =
        if inCurrentTraining database muscle'
          then "btn btn-secondary w-100"
          else "btn btn-primary w-100"
      viewButton :: Muscle -> L.Html ()
      viewButton muscle' = do
        L.a_ [L.href_ ("/training/" <> packShow muscle'), L.class_ (viewButtonClass muscle')] do
          L.span_ $ L.toHtml (packShow muscle')
      buttonArray :: [[Muscle]]
      buttonArray = chunksOf 2 allMuscles
  forM_ buttonArray \buttonList -> do
    L.div_ [L.class_ "row mb-3"] (forM_ buttonList (L.div_ [L.class_ "col-6 text-center"] . viewButton))

exerciseFormMusclesParam :: (IsString a) => a
exerciseFormMusclesParam = "muscles"

exerciseFormFilesToDeleteParam :: (IsString a) => a
exerciseFormFilesToDeleteParam = "filesToDelete"

exerciseFormCategoryParam :: (IsString a) => a
exerciseFormCategoryParam = "category"

exerciseFormNameParam :: (IsString a) => a
exerciseFormNameParam = "name"

exerciseFormDescriptionParam :: (IsString a) => a
exerciseFormDescriptionParam = "description"

idExerciseForm :: HtmlId
idExerciseForm = HtmlId "new-exercise-form"

exerciseFormHtml :: Exercise -> L.Html ()
exerciseFormHtml (Exercise {name, category, muscles, fileReferences, description}) =
  L.form_ [L.enctype_ "multipart/form-data", L.method_ "post", L.action_ "/edit-exercise"] do
    L.input_
      [ L.type_ "hidden",
        L.name_ "original-exercise-name",
        L.value_ (packShow name)
      ]
    L.div_ [L.class_ "form-floating mb-3"] do
      L.input_
        [ L.class_ "form-control",
          L.id_ "exercise-name",
          L.type_ "text",
          L.name_ exerciseFormNameParam,
          L.value_ (packShow name)
        ]
      L.label_ [L.for_ "exercise-name"] "Name"

    L.h5_ "Category"
    L.div_ [L.class_ "d-flex justify-content-evenly mb-3"] $ forM_ allCategories \category' -> do
      L.input_
        [ L.class_ "btn-check",
          L.id_ ("category-" <> htmlIdFromText (packShow category')),
          L.type_ "radio",
          L.name_ exerciseFormCategoryParam,
          if category' == category then L.checked_ else mempty,
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
          L.id_ ("muscle-" <> htmlIdFromText (packShow muscle')),
          L.type_ "checkbox",
          L.name_ exerciseFormMusclesParam,
          L.value_ (packShow muscle'),
          if muscle' `elem` muscles then L.checked_ else mempty
        ]
      L.label_
        [L.for_ ("muscle-" <> packShow muscle'), L.class_ "btn btn-outline-secondary me-2"]
        $ L.toHtml
        $ packShow muscle'

    L.textarea_
      [ L.class_ "form-control mb-3",
        L.placeholder_ "Description",
        L.name_ exerciseFormDescriptionParam
      ]
      (L.toHtml description)

    L.h5_ "Attached files"
    forM_ fileReferences \(FileReference fileRef) -> do
      L.div_ [L.class_ "d-flex flex-row mb-3 align-items-center justify-content-evenly"] do
        L.div_ do
          L.div_ [L.class_ "form-check"] do
            L.input_
              [ L.type_ "checkbox",
                L.class_ "form-check-input",
                L.name_ exerciseFormFilesToDeleteParam,
                L.id_ ("delete-" <> fileRef),
                L.value_ fileRef
              ]
            L.label_ [L.class_ "form-check-label", L.for_ ("delete-" <> fileRef)] "Delete this"
        L.div_ (exerciseImageHtml (FileReference fileRef))

    L.div_ [L.class_ "mb-3"] do
      L.label_ [L.for_ "file-upload", L.class_ "form-label"] "Files to attach"
      L.input_ [L.class_ "form-control", L.type_ "file", L.multiple_ "true", L.id_ "file-upload", L.name_ "attached-files"]

    L.div_ [L.class_ "hstack gap-3"] do
      L.button_
        [ L.type_ "submit",
          L.class_ "btn btn-primary"
        ]
        "Submit"
      L.button_
        [ L.type_ "submit",
          L.class_ "btn btn-outline-warning",
          L.formaction_ "/exercises",
          L.formmethod_ "get"
        ]
        "Cancel edit"

newExerciseButtonHtml :: L.Html ()
newExerciseButtonHtml =
  L.form_ [L.action_ "/exercises"] do
    L.input_
      [ L.type_ "hidden",
        L.name_ "with-form",
        L.value_ (packShow True)
      ]
    L.button_
      [ L.type_ "submit",
        L.class_ "btn btn-primary"
      ]
      do
        iconHtml "plus-lg"
        L.span_ "New exercise"

exerciseImageHtml :: FileReference -> L.Html ()
exerciseImageHtml (FileReference fileRef) = L.figure_ [L.class_ "figure"] $ L.img_ [L.src_ ("/" <> pack "uploaded-files" <> "/" <> fileRef), L.class_ "figure-img img-fluid rounded"]

exerciseDescriptionHtml :: Exercise -> L.Html ()
exerciseDescriptionHtml e = do
  L.toHtmlRaw $ commonmarkToHtml [] [] e.description
  forM_ e.fileReferences exerciseImageHtml

viewExerciseListOuter :: Database -> Maybe Exercise -> L.Html ()
viewExerciseListOuter db editedExercise =
  viewHtmlSkeleton PageExercises $ viewExerciseList db editedExercise

viewExerciseList :: Database -> Maybe Exercise -> L.Html ()
viewExerciseList db existingExercise = do
  L.div_ [makeId idExerciseForm, L.class_ "mb-3"] do
    maybe newExerciseButtonHtml exerciseFormHtml existingExercise
  L.hr_ []
  L.h2_ do
    iconHtml "box2-heart"
    L.span_ "Exercise Descriptions"
  forM_ db.exercises \exercise' -> do
    L.h3_ [L.id_ ("description-" <> htmlIdFromText (packShow exercise'.name)), L.class_ "d-flex"] do
      L.form_ [L.action_ "/exercises"] do
        L.input_ [L.type_ "hidden", L.name_ "edit-exercise", L.value_ (packShow exercise'.name)]
        L.button_
          [ L.class_ "btn btn-sm btn-secondary me-2",
            L.type_ "submit"
          ]
          (iconHtml' "pencil-square")
      L.form_ [L.action_ ("/remove-exercise/" <> packShow exercise'.name)] do
        L.button_
          [ L.class_ "btn btn-sm btn-danger me-2",
            L.type_ "submit"
          ]
          (iconHtml' "trash-fill")
      L.toHtml (packShow exercise'.name)
    L.div_ [L.class_ "gap-1 mb-3"] do
      L.span_ [L.class_ "badge text-bg-dark me-1"] (L.toHtml $ packShow exercise'.category)
      forM_ exercise'.muscles \muscle' -> L.span_ [L.class_ "badge text-bg-info me-1"] (L.toHtml $ packShow muscle')
    L.div_ [L.class_ "alert alert-light"] (exerciseDescriptionHtml exercise')

musclesAndLastExerciseSorted :: [ExerciseWithIntensity Exercise] -> [(Muscle, Maybe UTCTime)]
musclesAndLastExerciseSorted pastExercises =
  let musclesAndLastExercise :: [(Muscle, Maybe UTCTime)]
      musclesAndLastExercise =
        ( \muscle ->
            (muscle, (.time) <$> find (\ewi -> muscle `elem` ewi.exercise.muscles) (reverse pastExercises))
        )
          <$> allMuscles
   in -- compare by last execution and let "no execution" be "the beginning of time"
      sortBy
        (comparing (\(_muscle, time) -> fromMaybe (Data.Time.ModifiedJulianDay 0) (utctDay <$> time)))
        musclesAndLastExercise

exerciseHistoryForCategoryHtml :: UTCTime -> Database -> Category -> L.Html ()
exerciseHistoryForCategoryHtml currentTime db category =
  let pastExercises = filter (\pe -> pe.exercise.category == category) db.pastExercises
   in L.ol_ [L.class_ "list-group list-group-numbered"] $ forM_ (musclesAndLastExerciseSorted pastExercises) \(muscle, lastTime) -> do
        L.li_ [L.class_ "list-group-item d-flex justify-content-between align-items-start"] do
          L.div_ [L.class_ "fw-bold"] (L.toHtml $ packShow muscle)
          case lastTime of
            Just lastTime' -> L.toHtml $ dayDiffText currentTime lastTime'
            Nothing -> "never trained!"

exerciseHistoryHtml :: UTCTime -> Database -> L.Html ()
exerciseHistoryHtml currentTime db = do
  L.h1_ do
    iconHtml "clipboard-data-fill"
    L.span_ "Training state"
  L.div_ [L.class_ "row mb-3"] do
    L.div_ [L.class_ "col-6 text-center"] do
      L.h5_ "Anterior View"
      L.img_ [L.src_ "/muscles/front.svg", L.class_ "img-fluid"]
    L.div_ [L.class_ "col-6 text-center"] do
      L.h5_ "Posterior View"
      L.img_ [L.src_ "/muscles/back.svg", L.class_ "img-fluid"]
  exerciseHistoryForCategoryHtml currentTime db Strength

viewPageCurrentHtml :: UTCTime -> Database -> L.Html ()
viewPageCurrentHtml currentTime db = viewHtmlSkeleton PageCurrent $ do
  L.div_ [L.class_ "text-bg-light p-2"] do
    L.ul_ $ do
      L.li_ $ L.a_ [makeHref idCurrentWorkout] do
        iconHtml "joystick"
        L.span_ "Current"
      L.li_ $ L.a_ [makeHref idSoreness] do
        iconHtml "graph-down-arrow"
        L.span_ "Soreness"
      L.li_ $ L.a_ [makeHref idExerciseHistory] do
        iconHtml "clipboard-data-fill"
        L.span_ "Exercise History"
  viewCurrentWorkout db
  L.hr_ [L.class_ "mb-3"]
  sorenessInputAndOutput db
  L.hr_ [L.class_ "mb-3"]
  exerciseHistoryHtml currentTime db

viewChooseOuter :: Database -> L.Html ()
viewChooseOuter db = viewHtmlSkeleton PageChooseExercise (viewChoose db)

viewExerciseDeletion :: ExerciseName -> L.Html ()
viewExerciseDeletion exerciseName = viewHtmlSkeleton (PageExerciseDeletion exerciseName) do
  L.div_ [L.class_ "alert alert-danger"] do
    L.span_ do
      iconHtml "exclamation-circle-fill"
      "Are you sure you want to delete exercise "
      L.strong_ (L.toHtml $ packShow exerciseName)
    L.form_ [L.action_ ("/remove-exercise/" <> packShow exerciseName)] do
      L.input_ [L.type_ "hidden", L.name_ "sure", L.value_ "yes"]
      L.button_ [L.type_ "submit", L.class_ "btn btn-danger"] "Yes"
