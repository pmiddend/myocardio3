{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Views
  ( viewPageCurrentHtml,
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
import Data.Bool (Bool (True), not, otherwise)
import Data.Eq (Eq, (==))
import Data.Foldable (Foldable (elem), any, find, foldMap, forM_, for_, mapM_)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List (filter, zip)
import Data.List.Split (chunksOf)
import Data.Maybe (Maybe (Just, Nothing), fromMaybe, isJust, maybe)
import Data.Monoid (Monoid (mempty))
import Data.Ord (Ord ((<=)), comparing, (>))
import Data.Semigroup (Semigroup ((<>)))
import Data.Set qualified as Set
import Data.String (IsString)
import Data.Text (Text, pack, replace)
import Data.Time.Clock (UTCTime (utctDay, utctDayTime), diffUTCTime, nominalDay)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Tuple (fst)
import Lucid qualified as L
import Lucid.Base (makeAttributes)
import Myocardio.DatabaseNew (ExerciseWorkout (ExerciseWorkout), IdType, Muscle, MuscleWithWorkout (MuscleWithWorkout), Soreness, littleSore, verySore)
import Myocardio.DatabaseNew qualified as DBN
import Safe (lastMay, maximumByMay)
import Util (packShow)
import Prelude (Fractional ((/)), RealFrac (round), Show, succ)

iconHtml :: Text -> L.Html ()
iconHtml name' = L.i_ [L.class_ ("bi-" <> name' <> " me-2")] mempty

iconHtml' :: Text -> L.Html ()
iconHtml' name' = L.i_ [L.class_ ("bi-" <> name')] mempty

data CurrentPage
  = PageCurrent
  | PageStats
  | PageMuscle DBN.Muscle
  | PageExerciseDeletion IdType
  | PageChooseExercise
  | PageExercises
  deriving (Eq, Show)

pageToPath :: CurrentPage -> Text
pageToPath PageCurrent = "/"
pageToPath PageStats = "/stats"
pageToPath (PageExerciseDeletion name) = "/remove-exercise/" <> packShow name
pageToPath PageExercises = "/exercises"
pageToPath PageChooseExercise = "/training"
pageToPath (PageMuscle m) = "/training/" <> packShow m

-- instance Parsable CurrentPage where
--   parseParam = readEither

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

sorenessValueToEmoji :: Int -> Text
sorenessValueToEmoji 0 = ""
sorenessValueToEmoji 1 = "😕"
sorenessValueToEmoji _ = "😭"

sorenessOutput :: [DBN.Soreness] -> L.Html ()
sorenessOutput soreness = do
  let sorenessToHtml :: DBN.Soreness -> L.Html ()
      sorenessToHtml soreness' = L.li_ $ L.form_ [L.action_ "/reset-soreness", L.method_ "post"] do
        L.input_ [L.type_ "hidden", L.name_ "muscle", L.value_ (packShow soreness'.muscleId)]
        L.span_ [L.class_ "me-1"] (L.toHtml (sorenessValueToEmoji soreness'.soreness))
        L.strong_ [L.class_ "me-1"] (L.toHtml soreness'.muscleName)
        L.button_
          [L.type_ "submit", L.class_ "btn btn-link"]
          "Reset"
  L.div_ [L.id_ "soreness-output"] do
    L.ul_ (mapM_ sorenessToHtml soreness)

idCurrentWorkout :: HtmlId
idCurrentWorkout = HtmlId "current-workout"

idSoreness :: HtmlId
idSoreness = HtmlId "soreness"

idExerciseHistory :: HtmlId
idExerciseHistory = HtmlId "exercise-history"

sorenessInputAndOutput :: [Muscle] -> [DBN.Soreness] -> L.Html ()
sorenessInputAndOutput allMuscles soreness = do
  L.h1_ [makeId idSoreness] do
    iconHtml "graph-down-arrow"
    L.span_ "Soreness"
  L.form_ [L.action_ "/update-soreness", L.method_ "post"] do
    L.div_ [L.class_ "mb-2"] do
      L.div_ [L.class_ "form-floating"] do
        let muscleOption :: DBN.Muscle -> L.Html ()
            muscleOption muscle' = L.option_ [L.value_ (packShow muscle'.id)] (L.toHtml muscle'.name)
        L.select_ [L.class_ "form-select", L.id_ "i-am-sore", L.name_ "muscle"] (mapM_ muscleOption allMuscles)
        L.label_ [L.for_ "i-am-sore"] "I am sore here"
    L.div_ [L.class_ "d-flex justify-content-evenly align-items-center mb-2"] do
      L.input_
        [ L.class_ "btn-check",
          L.type_ "radio",
          L.name_ "how-sore",
          L.id_ "verysore",
          L.value_ (packShow verySore),
          L.checked_
        ]
      L.label_ [L.class_ "btn", L.for_ "verysore"] (L.toHtml $ sorenessValueToEmoji 2 <> " VERY")
      L.input_
        [ L.class_ "btn-check",
          L.type_ "radio",
          L.name_ "how-sore",
          L.id_ "alittle",
          L.value_ (packShow littleSore)
        ]
      L.label_ [L.class_ "btn", L.for_ "alittle"] (L.toHtml $ sorenessValueToEmoji 1 <> " A LITTLE")
    L.div_ [L.class_ "d-flex justify-content-center"] do
      L.button_
        [ L.class_ "btn btn-primary",
          L.type_ "submit"
        ]
        "Submit"
  sorenessOutput soreness

dayDiffText :: UTCTime -> UTCTime -> Text
dayDiffText currentTime before =
  let dayDiff = round $ diffUTCTime currentTime before / nominalDay
   in case dayDiff :: Int of
        0 -> "today"
        1 -> "yesterday"
        n -> packShow n <> " days ago"

viewExerciseImageCarousel :: DBN.ExerciseWithWorkouts -> L.Html ()
viewExerciseImageCarousel exercise = do
  L.div_
    [ L.class_ "carousel slide",
      L.id_ ("carousel-" <> htmlIdFromText (packShow exercise.name)),
      makeAttributes "data-bs-theme" "dark"
    ]
    $ do
      L.div_
        [L.class_ "carousel-inner"]
        ( forM_ (zip [0 ..] (Set.toList exercise.fileIds)) \(i, fileId) ->
            L.div_
              [ L.class_ "carousel-item" <> if i == (0 :: Int) then L.class_ "active" else mempty
              ]
              $ L.img_ [L.src_ ("/uploaded-files/" <> packShow fileId), L.class_ "d-block w-100"]
        )
      unless (Set.size exercise.fileIds <= 1) do
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

viewSingleExerciseInCurrentWorkout :: DBN.ExerciseWithWorkouts -> L.Html ()
viewSingleExerciseInCurrentWorkout exWithIn = L.div_ [L.class_ "mb-3 card"] do
  viewExerciseImageCarousel exWithIn
  L.div_ [L.class_ "card-body"] do
    L.h5_ [L.class_ "card-title"] do
      L.strong_ (L.toHtml exWithIn.name)
    L.h6_ [L.class_ "card-subtitle"] do
      for_ (Set.lookupMin exWithIn.workouts) \(ExerciseWorkout {intensity}) ->
        "Intensity: " <> L.strong_ (L.toHtml intensity)
    L.p_ [L.class_ "card-text text-info"] do
      L.toHtmlRaw $ commonmarkToHtml [] [] exWithIn.description
      L.form_ [L.action_ "/toggle-exercise-in-workout", L.method_ "post"] do
        L.input_ [L.type_ "hidden", L.name_ "return-to-current", L.value_ (packShow True)]
        L.input_ [L.value_ "", L.name_ "intensity", L.type_ "hidden"]
        L.input_ [L.type_ "hidden", L.name_ "exercise-id", L.value_ (packShow exWithIn.id)]
        L.button_
          [ L.type_ "submit",
            L.class_ "btn btn-secondary btn-sm"
          ]
          do
            iconHtml "trash"
            L.span_ "Remove from workout"
      L.form_ [L.action_ "/change-intensity", L.method_ "post"] do
        L.input_ [L.type_ "hidden", L.name_ "exercise-id", L.value_ (packShow exWithIn.id)]
        for_ (Set.lookupMin exWithIn.workouts) \(ExerciseWorkout {intensity}) ->
          L.input_
            [ L.class_ "form-control form-control-sm mb-1",
              L.value_ intensity,
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

viewCurrentWorkout :: [DBN.Muscle] -> [DBN.ExerciseWithWorkouts] -> L.Html ()
viewCurrentWorkout allMuscles' exercises =
  L.div_ [makeId idCurrentWorkout] do
    case exercises of
      [] -> mempty
      currentExercises -> do
        L.h1_ do
          iconHtml "joystick"
          L.span_ "Current"
        let musclesInvolved :: Set.Set Muscle
            musclesInvolved = foldMap (\e -> e.muscles) currentExercises
            musclesMissing :: Set.Set Muscle
            musclesMissing = Set.fromList allMuscles' `Set.difference` musclesInvolved
        L.div_ [L.class_ "gap-1 mb-3"] do
          L.span_ [L.class_ "text-muted me-1"] "Trained: "
          forM_ musclesInvolved \muscle' -> L.span_ [L.class_ "badge text-bg-success me-1"] (L.toHtml muscle'.name)
        L.div_ [L.class_ "gap-1 mb-3"] do
          L.span_ [L.class_ "text-muted me-1"] "Missing: "
          forM_ musclesMissing \muscle' -> L.span_ [L.class_ "badge text-bg-warning me-1"] (L.toHtml muscle'.name)
        forM_ (chunksOf 2 currentExercises) \exerciseRow -> do
          L.div_ [L.class_ "row"] do
            forM_ exerciseRow (L.div_ [L.class_ "col-lg-6 col-12"] . viewSingleExerciseInCurrentWorkout)

        L.form_ [L.action_ "/commit-workout", L.method_ "post"] do
          L.button_
            [ L.type_ "submit",
              L.class_ "btn btn-success"
            ]
            do
              iconHtml "send"
              L.span_ "Commit workout"

inCurrentTraining :: [DBN.ExerciseWithWorkouts] -> DBN.Muscle -> Bool
inCurrentTraining currentTraining muscle =
  isJust (find (\ewi -> muscle `Set.member` ewi.muscles) currentTraining)

firstSorenessAfterLastExecution :: Muscle -> [Soreness] -> ExerciseWorkout -> Maybe Soreness
firstSorenessAfterLastExecution muscle' sorenessHistory lastExecution =
  let beginningOfDayAfterLastWorkout :: UTCTime
      beginningOfDayAfterLastWorkout = lastExecution.time {utctDay = succ lastExecution.time.utctDay, utctDayTime = 1}
      sorenessHistoryForThisMuscle = filter (\soreness -> soreness.muscleId == muscle'.id) sorenessHistory
   in find
        (\soreness -> soreness.time > beginningOfDayAfterLastWorkout)
        sorenessHistoryForThisMuscle

viewSingleExerciseInChooser :: UTCTime -> Muscle -> [DBN.Soreness] -> DBN.ExerciseWithWorkouts -> L.Html ()
viewSingleExerciseInChooser currentTime muscle' sorenessHistory exerciseWithWorkouts = do
  let lastExecutionOfThisExercise :: Maybe DBN.ExerciseWorkout
      lastExecutionOfThisExercise =
        lastMay (Set.toList exerciseWithWorkouts.workouts)
      partOfCurrentWorkout :: Bool
      partOfCurrentWorkout = any (\workout -> not workout.committed) exerciseWithWorkouts.workouts
      viewLastExecution = case lastExecutionOfThisExercise of
        Nothing -> L.p_ "Never executed!"
        Just lastExecutionInstance ->
          do
            L.span_ $ L.toHtml $ "Last: " <> dayDiffText currentTime lastExecutionInstance.time
            L.br_ []
            case firstSorenessAfterLastExecution muscle' sorenessHistory lastExecutionInstance of
              Nothing -> L.span_ [L.class_ "text-danger"] "Soreness unresolved!"
              Just lastSoreness ->
                if lastSoreness.soreness == DBN.notSore
                  then L.span_ [L.class_ "text-success"] $ L.toHtml $ "Soreness: " <> sorenessValueToEmoji lastSoreness.soreness
                  else L.span_ $ L.toHtml $ "Soreness: " <> sorenessValueToEmoji lastSoreness.soreness
            L.br_ []
            L.table_ [L.class_ "table table-sm"] do
              L.thead_ do
                L.tr_ do
                  L.th_ "When"
                  L.th_ "Comment"
              L.tbody_ do
                forM_ exerciseWithWorkouts.workouts \workout ->
                  L.tr_ do
                    L.td_ [L.class_ "text-nowrap"] $ L.toHtml $ pack $ formatTime defaultTimeLocale "%F" workout.time
                    L.td_ (L.toHtml workout.intensity)
  L.form_ [L.method_ "post", L.action_ "/toggle-exercise-in-workout"] do
    L.input_
      [ L.type_ "hidden",
        L.name_ "exercise-id",
        L.value_ (packShow exerciseWithWorkouts.id)
      ]
    L.div_ [L.class_ "mb-3 card"] do
      viewExerciseImageCarousel exerciseWithWorkouts
      L.div_ [L.class_ "card-body"] do
        L.h5_ [L.class_ "card-title"] do
          L.span_ $ L.toHtml exerciseWithWorkouts.name
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
              L.div_ [L.class_ "text-muted mb-3"] viewLastExecution
              L.div_ [L.class_ "mb-3"] do
                L.input_
                  [ L.class_ "form-control mb-1",
                    L.value_ (maybe "" (.intensity) lastExecutionOfThisExercise),
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

viewConcreteMuscleGroupExercisesOuter :: UTCTime -> [Soreness] -> [DBN.ExerciseWithWorkouts] -> Muscle -> L.Html ()
viewConcreteMuscleGroupExercisesOuter currentTime sorenessHistory allExercises muscle =
  viewHtmlSkeleton
    (PageMuscle muscle)
    (viewConcreteMuscleGroupExercises currentTime sorenessHistory allExercises muscle)

viewConcreteMuscleGroupExercises :: UTCTime -> [Soreness] -> [DBN.ExerciseWithWorkouts] -> DBN.Muscle -> L.Html ()
viewConcreteMuscleGroupExercises currentTime sorenessHistory allExercises muscle =
  case filter (\m -> muscle `Set.member` m.muscles) allExercises of
    [] -> mempty
    exercisesForThisMuscle ->
      let lastTraining :: L.Html ()
          lastTraining =
            case maximumByMay
              (comparing fst)
              -- Map the list of exercises with workouts nested, into
              -- a flat list of exercise name and workout time.
              -- Tailor-made to then display it.
              (exercisesForThisMuscle >>= (\exerciseWithWorkouts -> (\workout -> (workout.time, exerciseWithWorkouts.name)) <$> Set.toList exerciseWithWorkouts.workouts)) of
              Nothing -> L.p_ do
                L.em_ "never trained!"
              Just (workoutTime, exerciseName) ->
                L.p_ [L.class_ "text-muted"] do
                  L.em_ $ do
                    L.toHtml $ "Last training: " <> dayDiffText currentTime workoutTime <> ": "
                    L.strong_ $ L.toHtml $ packShow exerciseName
       in do
            L.h2_
              [ L.id_ ("training-section-" <> htmlIdFromText (packShow muscle.id)),
                L.class_ "mt-3"
              ]
              (L.toHtml muscle.name)
            L.div_ do
              lastTraining
              L.div_ do
                forM_ (chunksOf 2 exercisesForThisMuscle) \exerciseRow -> do
                  L.div_ [L.class_ "row"] do
                    forM_ exerciseRow (L.div_ [L.class_ "col-lg-6 col-12"] . viewSingleExerciseInChooser currentTime muscle sorenessHistory)

viewChoose :: [DBN.Muscle] -> [DBN.Soreness] -> [DBN.ExerciseWithWorkouts] -> L.Html ()
viewChoose allMuscles' sorenessHistory currentTraining = do
  let currentMuscleSoreness :: DBN.Muscle -> Int
      currentMuscleSoreness muscle =
        fromMaybe 0 ((\s -> s.soreness) <$> find (\s -> s.muscleId == muscle.id) sorenessHistory)
      viewButtonClass muscle'
        | inCurrentTraining currentTraining muscle' =
            if currentMuscleSoreness muscle' == 0
              then "btn btn-secondary w-100"
              else "btn btn-danger w-100"
        | currentMuscleSoreness muscle' == 0 = "btn btn-primary w-100"
        | otherwise = "btn btn-warning w-100"
      viewButton :: DBN.Muscle -> L.Html ()
      viewButton muscle' = do
        L.a_ [L.href_ ("/training/" <> packShow muscle'.id), L.class_ (viewButtonClass muscle')] do
          L.span_ $ L.toHtml muscle'.name
      buttonArray :: [[DBN.Muscle]]
      buttonArray = chunksOf 2 allMuscles'
  forM_ buttonArray \buttonList -> do
    L.div_ [L.class_ "row mb-3"] (forM_ buttonList (L.div_ [L.class_ "col-6 text-center"] . viewButton))
  L.div_ [L.class_ "d-flex gap-3 mb-1"] do
    L.button_ [L.type_ "button", L.class_ "btn btn-primary"] (L.span_ " ")
    L.span_ (L.toHtml ("not in workout" :: Text))
  L.div_ [L.class_ "d-flex gap-3 mb-1"] do
    L.button_ [L.type_ "button", L.class_ "btn btn-secondary"] (L.span_ " ")
    L.span_ (L.toHtml ("in workout" :: Text))
  L.div_ [L.class_ "d-flex gap-3 mb-1"] do
    L.button_ [L.type_ "button", L.class_ "btn btn-warning"] (L.span_ " ")
    L.span_ (L.toHtml ("sore muscle" :: Text))
  L.div_ [L.class_ "d-flex gap-3 mb-1"] do
    L.button_ [L.type_ "button", L.class_ "btn btn-danger"] (L.span_ " ")
    L.span_ (L.toHtml ("sore muscle in workout" :: Text))

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

viewExerciseFormHtml :: [DBN.Muscle] -> DBN.ExerciseDescription -> L.Html ()
viewExerciseFormHtml allMuscles' (DBN.ExerciseDescription {id, name, muscles, fileIds, description}) =
  L.form_ [L.enctype_ "multipart/form-data", L.method_ "post", L.action_ "/edit-exercise"] do
    when (id > 0) (L.input_ [L.type_ "hidden", L.name_ "exercise-id", L.value_ (packShow id)])
    L.div_ [L.class_ "form-floating mb-3"] do
      L.input_
        [ L.class_ "form-control",
          L.id_ "exercise-name",
          L.type_ "text",
          L.name_ exerciseFormNameParam,
          L.value_ name
        ]
      L.label_ [L.for_ "exercise-name"] "Name"

    L.h5_ "Muscles involved"
    L.div_ [L.class_ "mb-3"] $ forM_ allMuscles' \muscle' -> do
      L.input_
        [ L.class_ "btn-check",
          L.id_ ("muscle-" <> packShow muscle'.id),
          L.type_ "checkbox",
          L.name_ exerciseFormMusclesParam,
          L.value_ (packShow muscle'.id),
          if muscle' `elem` muscles then L.checked_ else mempty
        ]
      L.label_
        [L.for_ ("muscle-" <> packShow muscle'.id), L.class_ "btn btn-outline-secondary me-2"]
        $ L.toHtml muscle'.name

    L.textarea_
      [ L.class_ "form-control mb-3",
        L.placeholder_ "Description",
        L.name_ exerciseFormDescriptionParam
      ]
      (L.toHtml description)

    L.h5_ "Attached files"
    forM_ fileIds \fileId -> do
      L.div_ [L.class_ "d-flex flex-row mb-3 align-items-center justify-content-evenly"] do
        L.div_ do
          L.div_ [L.class_ "form-check"] do
            L.input_
              [ L.type_ "checkbox",
                L.class_ "form-check-input",
                L.name_ exerciseFormFilesToDeleteParam,
                L.id_ ("delete-" <> packShow fileId),
                L.value_ (packShow fileId)
              ]
            L.label_ [L.class_ "form-check-label", L.for_ ("delete-" <> packShow fileId)] "Delete this"
        L.div_ (viewExerciseImageHtml fileId)

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

viewExerciseImageHtml :: IdType -> L.Html ()
viewExerciseImageHtml fileId =
  L.figure_ [L.class_ "figure"] $
    L.img_ [L.src_ ("/" <> pack "uploaded-files" <> "/" <> packShow fileId), L.class_ "figure-img img-fluid rounded"]

viewExerciseDescriptionHtml :: DBN.ExerciseDescription -> L.Html ()
viewExerciseDescriptionHtml e = do
  L.toHtmlRaw $ commonmarkToHtml [] [] e.description
  forM_ e.fileIds viewExerciseImageHtml

viewExerciseListOuter :: [DBN.Muscle] -> [DBN.ExerciseDescription] -> Maybe DBN.ExerciseDescription -> L.Html ()
viewExerciseListOuter allMuscles exercises editedExercise =
  viewHtmlSkeleton PageExercises $ viewExerciseList allMuscles exercises editedExercise

viewExerciseList :: [DBN.Muscle] -> [DBN.ExerciseDescription] -> Maybe DBN.ExerciseDescription -> L.Html ()
viewExerciseList allMuscles' exercises existingExercise = do
  L.div_ [makeId idExerciseForm, L.class_ "mb-3"] do
    maybe newExerciseButtonHtml (viewExerciseFormHtml allMuscles') existingExercise
  L.hr_ []
  L.h2_ do
    iconHtml "box2-heart"
    L.span_ "Exercise Descriptions"
  forM_ exercises \exercise' -> do
    L.h3_ [L.id_ ("description-" <> htmlIdFromText exercise'.name), L.class_ "d-flex"] do
      L.form_ [L.action_ "/exercises"] do
        L.input_ [L.type_ "hidden", L.name_ "edit-exercise", L.value_ (packShow exercise'.id)]
        L.button_
          [ L.class_ "btn btn-sm btn-secondary me-2",
            L.type_ "submit"
          ]
          (iconHtml' "pencil-square")
      L.form_ [L.action_ ("/remove-exercise/" <> packShow exercise'.id)] do
        L.button_
          [ L.class_ "btn btn-sm btn-danger me-2",
            L.type_ "submit"
          ]
          (iconHtml' "trash-fill")
      L.toHtml exercise'.name
    L.div_ [L.class_ "gap-1 mb-3"] do
      forM_ exercise'.muscles \muscle' -> L.span_ [L.class_ "badge text-bg-info me-1"] (L.toHtml muscle'.name)
    L.div_ [L.class_ "alert alert-light"] (viewExerciseDescriptionHtml exercise')

viewExerciseHistory :: UTCTime -> [MuscleWithWorkout] -> L.Html ()
viewExerciseHistory currentTime musclesWithLastWorkout = do
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
  L.ol_ [L.class_ "list-group list-group-numbered"] $
    forM_ musclesWithLastWorkout \(MuscleWithWorkout {muscleName, workoutTime}) -> do
      L.li_ [L.class_ "list-group-item d-flex justify-content-between align-items-start"] do
        L.div_ [L.class_ "fw-bold"] (L.toHtml muscleName)
        L.toHtml $ dayDiffText currentTime workoutTime

viewPageCurrentHtml :: [DBN.Muscle] -> [DBN.ExerciseWithWorkouts] -> [DBN.Soreness] -> L.Html ()
viewPageCurrentHtml allMuscles' exercises sorenessHistory = viewHtmlSkeleton PageCurrent $ do
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
  viewCurrentWorkout allMuscles' exercises
  L.hr_ [L.class_ "mb-3"]
  sorenessInputAndOutput allMuscles' sorenessHistory

viewChooseOuter :: [DBN.Muscle] -> [DBN.Soreness] -> [DBN.ExerciseWithWorkouts] -> L.Html ()
viewChooseOuter allMuscles' sorenessHistory currentTraining =
  viewHtmlSkeleton
    PageChooseExercise
    ( viewChoose allMuscles' sorenessHistory currentTraining
    )

viewExerciseDeletion :: IdType -> Text -> L.Html ()
viewExerciseDeletion exerciseId name = viewHtmlSkeleton (PageExerciseDeletion exerciseId) do
  L.div_ [L.class_ "alert alert-danger"] do
    L.span_ do
      iconHtml "exclamation-circle-fill"
      "Are you sure you want to delete exercise "
      L.strong_ (L.toHtml name)
    L.form_ [L.action_ ("/remove-exercise/" <> packShow exerciseId)] do
      L.input_ [L.type_ "hidden", L.name_ "sure", L.value_ "yes"]
      L.button_ [L.type_ "submit", L.class_ "btn btn-danger"] "Yes"
