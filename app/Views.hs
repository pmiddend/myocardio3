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
    viewStats,
    exerciseFormMusclesParam,
    exerciseFormFilesToDeleteParam,
    exerciseFormCategoryParam,
    exerciseFormDescriptionParam,
    exerciseFormNameParam,
    muscleIdForMuscleSorenessFromHtml,
  )
where

import CMarkGFM (commonmarkToHtml)
import Control.Monad (unless, void, when, (>>=))
import Data.Bool (Bool (True), not, otherwise, (&&))
import Data.Either (Either (Left, Right))
import Data.Eq (Eq, (/=), (==))
import Data.Foldable (Foldable (elem), any, find, foldMap, foldr, forM_, for_, mapM_)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int, Int64)
import Data.List (filter, sortOn, zip)
import Data.List.Split (chunksOf)
import Data.Map.Strict qualified as Map
import Data.Maybe (Maybe (Just, Nothing), fromMaybe, isJust, maybe)
import Data.Monoid (Monoid (mempty))
import Data.Ord (Ord ((<=)), comparing, max, min, (<), (>), (>=))
import Data.Semigroup (Semigroup ((<>)))
import Data.Set qualified as Set
import Data.String (IsString)
import Data.Text (Text, breakOnEnd, intercalate, pack, replace)
import Data.Text.Lazy qualified as TL
import Data.Text.Read (decimal)
import Data.Time.Clock (UTCTime (utctDay, utctDayTime), diffUTCTime, nominalDay)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Traversable (forM)
import Data.Tuple (fst, snd)
import Lucid qualified as L
import Lucid.Base (makeAttributes)
import Myocardio.DatabaseNew (ExerciseWorkout (ExerciseWorkout), IdType, Muscle, Soreness, SorenessScalar (LittleSore, NotSore, VerySore), sorenessScalarToInt)
import Myocardio.DatabaseNew qualified as DBN
import Safe (maximumByMay)
import Safe.Foldable (minimumMay)
import Text.Printf (printf)
import Util (packShow)
import Prelude (Double, Fractional ((/)), RealFrac (round), Show, isNaN, succ, (*))

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

sorenessValueToEmoji :: DBN.SorenessScalar -> Text
sorenessValueToEmoji NotSore = "😌"
sorenessValueToEmoji LittleSore = "😕"
sorenessValueToEmoji VerySore = "😭"

idCurrentWorkout :: HtmlId
idCurrentWorkout = HtmlId "current-workout"

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
              $ L.img_ [L.src_ (buildImageSrc fileId), L.class_ "d-block w-100"]
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

viewSingleExerciseInChooser :: UTCTime -> Muscle -> [DBN.ExerciseWithWorkouts] -> [DBN.Soreness] -> [DBN.Soreness] -> DBN.ExerciseWithWorkouts -> L.Html ()
viewSingleExerciseInChooser currentTime _muscle exercisesForThisMuscle sorenessHistory currentSoreness exerciseWithWorkouts = do
  let lastExecutionOfThisExercise :: Maybe DBN.ExerciseWorkout
      lastExecutionOfThisExercise =
        maximumByMay (comparing (.time)) (Set.toList exerciseWithWorkouts.workouts)
      beginningOfDayAfterLastWorkout :: Maybe UTCTime
      beginningOfDayAfterLastWorkout = (\le -> le.time {utctDay = succ le.time.utctDay, utctDayTime = 1}) <$> lastExecutionOfThisExercise
      workoutsForThisMuscleAfterLastOne :: [DBN.ExerciseWorkout]
      workoutsForThisMuscleAfterLastOne =
        case beginningOfDayAfterLastWorkout of
          -- We never had a workout with this exercise
          Nothing -> []
          Just beginningOfDayAfterLastWorkout' ->
            filter
              (\workout -> workout.time >= beginningOfDayAfterLastWorkout')
              (exercisesForThisMuscle >>= (Set.toList . (.workouts)))
      nextTimeThisMuscleWasWorked :: Maybe UTCTime
      nextTimeThisMuscleWasWorked = minimumMay ((.time) <$> workoutsForThisMuscleAfterLastOne)
      sorenessValuesInbetweenWorkouts :: [Soreness]
      sorenessValuesInbetweenWorkouts =
        case beginningOfDayAfterLastWorkout of
          Nothing -> []
          Just beginningOfDayAfterLastWorkout' ->
            case nextTimeThisMuscleWasWorked of
              -- We had a workout, but no subsequent one for this muscle
              Nothing -> filter (\soreness -> soreness.time >= beginningOfDayAfterLastWorkout') sorenessHistory
              Just nextTimeThisMuscleWasWorked' ->
                -- We had a workout, and another one after that (for this muscle)
                filter
                  (\soreness -> soreness.time >= beginningOfDayAfterLastWorkout' && soreness.time <= nextTimeThisMuscleWasWorked')
                  sorenessHistory
      partOfCurrentWorkout :: Bool
      partOfCurrentWorkout = any (\workout -> not workout.committed) exerciseWithWorkouts.workouts
      soreMusclesInThisExercise :: Set.Set Muscle
      soreMusclesInThisExercise = foldMap (Set.singleton . (.muscle)) currentSoreness `Set.intersection` exerciseWithWorkouts.muscles
      viewLastExecution = case lastExecutionOfThisExercise of
        Nothing -> L.p_ "Never executed!"
        Just lastExecutionInstance ->
          do
            L.span_ $ L.toHtml $ "Last: " <> dayDiffText currentTime lastExecutionInstance.time
            L.br_ []
            case maximumByMay (comparing (.soreness)) sorenessValuesInbetweenWorkouts of
              Nothing -> L.span_ [L.class_ "text-danger"] "Soreness unresolved!"
              Just lastSoreness ->
                if lastSoreness.soreness == DBN.NotSore
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
          if Set.null soreMusclesInThisExercise
            then mempty
            else
              L.span_
                [L.class_ "text-danger"]
                ( L.toHtml
                    ( "Contains sore muscles: "
                        <> intercalate ", " ((.name) <$> Set.toList soreMusclesInThisExercise)
                    )
                )
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

viewConcreteMuscleGroupExercisesOuter :: UTCTime -> [Soreness] -> [Soreness] -> [DBN.ExerciseWithWorkouts] -> Muscle -> L.Html ()
viewConcreteMuscleGroupExercisesOuter currentTime sorenessHistory currentSoreness allExercises muscle =
  viewHtmlSkeleton
    (PageMuscle muscle)
    (viewConcreteMuscleGroupExercises currentTime sorenessHistory currentSoreness allExercises muscle)

viewConcreteMuscleGroupExercises :: UTCTime -> [Soreness] -> [Soreness] -> [DBN.ExerciseWithWorkouts] -> DBN.Muscle -> L.Html ()
viewConcreteMuscleGroupExercises currentTime sorenessHistory currentSoreness allExercises muscle =
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
                    forM_ exerciseRow (L.div_ [L.class_ "col-lg-6 col-12"] . viewSingleExerciseInChooser currentTime muscle exercisesForThisMuscle sorenessHistory currentSoreness)

viewChoose :: [DBN.Muscle] -> [DBN.Soreness] -> [DBN.ExerciseWithWorkouts] -> L.Html ()
viewChoose allMuscles' currentSoreness currentTraining = do
  let currentMuscleSoreness :: DBN.Muscle -> DBN.SorenessScalar
      currentMuscleSoreness muscle =
        fromMaybe NotSore ((\s -> s.soreness) <$> find (\s -> s.muscle == muscle) currentSoreness)
      viewButtonClass muscle'
        | inCurrentTraining currentTraining muscle' =
            if currentMuscleSoreness muscle' == NotSore
              then "btn btn-secondary w-100"
              else "btn btn-danger w-100"
        | currentMuscleSoreness muscle' == NotSore = "btn btn-primary w-100"
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

buildImageSrc :: (Show a) => a -> Text
buildImageSrc fileId = "/" <> pack "uploaded-files" <> "/" <> packShow fileId

viewExerciseImageHtml :: IdType -> L.Html ()
viewExerciseImageHtml fileId =
  L.figure_ [L.class_ "figure"] $
    L.img_ [L.src_ (buildImageSrc fileId), L.class_ "figure-img img-fluid rounded"]

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
  void $ L.div_ [L.class_ "card text-bg-primary mb-3"] do
    L.div_ [L.class_ "card-header"] "TOC"
    L.div_ [L.class_ "card-body"] do
      L.ul_ $
        forM exercises \exercise -> L.li_ do
          L.a_ [L.class_ "text-bg-primary", L.href_ ("#ex" <> packShow exercise.id)] (L.toHtml exercise.name)
  forM_ exercises \exercise' -> do
    L.h3_ [L.id_ ("description-" <> htmlIdFromText exercise'.name), L.class_ "d-flex"] do
      L.a_ [L.name_ ("ex" <> packShow exercise'.id)] mempty
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
      forM_ exercise'.muscles \muscle' -> L.span_ [L.class_ "badge text-bg-primary me-1"] (L.toHtml muscle'.name)
    L.div_ [L.class_ "alert alert-light"] (viewExerciseDescriptionHtml exercise')

viewPageCurrentHtml :: UTCTime -> [DBN.Muscle] -> [DBN.ExerciseWithWorkouts] -> [DBN.ExerciseWithWorkouts] -> [DBN.Soreness] -> [DBN.Muscle] -> Bool -> L.Html ()
viewPageCurrentHtml currentTime allMuscles' exercises lastWorkout currentSoreness musclesTrainedHistory sorenessWasUpdated = viewHtmlSkeleton PageCurrent $ do
  viewCurrentWorkout allMuscles' exercises
  L.hr_ [L.class_ "mb-3"]
  viewLastWorkout currentTime lastWorkout currentSoreness musclesTrainedHistory sorenessWasUpdated

htmlIdForMuscleSoreness :: Muscle -> Text
htmlIdForMuscleSoreness muscle = "how-sore" <> packShow muscle.id

muscleIdForMuscleSorenessFromHtml :: Text -> Maybe Int64
muscleIdForMuscleSorenessFromHtml x =
  case breakOnEnd "how-sore" x of
    ("how-sore", muscleIdStr) ->
      case decimal muscleIdStr of
        Left _errorString -> Nothing
        Right (muscleId, _remainder) -> Just muscleId
    _ -> Nothing

viewSorenessForm :: (Foldable f) => f DBN.Muscle -> [DBN.Soreness] -> L.Html ()
viewSorenessForm musclesInvolved currentSoreness = do
  L.table_ do
    L.tbody_ do
      forM_ musclesInvolved \muscle' -> L.tr_ do
        L.td_ (L.toHtml muscle'.name)
        L.td_ do
          L.div_ [L.class_ "btn-group"] do
            L.input_
              [ L.class_ "btn-check",
                L.type_ "radio",
                L.name_ (htmlIdForMuscleSoreness muscle'),
                L.id_ ("notsore" <> packShow muscle'.id),
                L.value_ (packShow @Int (sorenessScalarToInt NotSore)),
                if any (\soreness -> soreness.muscle.id == muscle'.id && soreness.soreness /= NotSore) currentSoreness
                  then mempty
                  else L.checked_
              ]
            L.label_ [L.class_ "btn btn-outline-success", L.for_ ("notsore" <> packShow muscle'.id)] (L.toHtml $ sorenessValueToEmoji NotSore <> " NOT")
            L.input_
              [ L.class_ "btn-check",
                L.type_ "radio",
                L.name_ (htmlIdForMuscleSoreness muscle'),
                L.id_ ("alittle" <> packShow muscle'.id),
                L.value_ (packShow @Int (sorenessScalarToInt LittleSore)),
                if any (\soreness -> soreness.muscle.id == muscle'.id && soreness.soreness == LittleSore) currentSoreness
                  then L.checked_
                  else mempty
              ]
            L.label_ [L.class_ "btn btn-outline-warning", L.for_ ("alittle" <> packShow muscle'.id)] (L.toHtml $ sorenessValueToEmoji LittleSore <> " MEH")
            L.input_
              [ L.class_ "btn-check",
                L.type_ "radio",
                L.name_ (htmlIdForMuscleSoreness muscle'),
                L.id_ ("verysore" <> packShow muscle'.id),
                L.value_ (packShow @Int (sorenessScalarToInt VerySore)),
                if any (\soreness -> soreness.muscle.id == muscle'.id && soreness.soreness == VerySore) currentSoreness
                  then L.checked_
                  else mempty
              ]
            L.label_ [L.class_ "btn btn-outline-danger", L.for_ ("verysore" <> packShow muscle'.id)] (L.toHtml $ sorenessValueToEmoji VerySore <> " VERY")

viewLastWorkout :: UTCTime -> [DBN.ExerciseWithWorkouts] -> [DBN.Soreness] -> [DBN.Muscle] -> Bool -> L.Html ()
viewLastWorkout _ [] _ _ _ = mempty
viewLastWorkout currentTime exercises@(e : _) currentSoreness musclesTrainedHistory sorenessWasUpdated = case Set.elems e.workouts of
  [] -> mempty
  (workout : _) -> do
    L.h4_ do
      "Last workout: "
      L.em_ (L.toHtml (dayDiffText currentTime workout.time))
    L.form_ [L.action_ "/update-soreness", L.method_ "post"] do
      L.div_ [L.class_ "gap-1 mb-3"] do
        L.span_ [L.class_ "text-muted me-1"] "Trained: "
        let musclesInLastWorkout = foldMap (.muscles) exercises
            historicalMusclesOrSore =
              Set.fromList musclesTrainedHistory
                <> foldr (Set.insert . (.muscle)) mempty currentSoreness
        viewSorenessForm musclesInLastWorkout currentSoreness
        let musclesRemaining = historicalMusclesOrSore `Set.difference` musclesInLastWorkout
        when (not (Set.null musclesRemaining)) do
          L.span_ [L.class_ "text-muted me-1"] "Historical: "
          viewSorenessForm musclesRemaining currentSoreness

        L.div_ do
          L.button_ [L.class_ "btn btn-primary mt-3", L.type_ "submit"] "💪 Update soreness"

        when sorenessWasUpdated do
          L.div_ [L.class_ "badge text-bg-success"] "Soreness updated!"

      L.div_ [L.class_ "gap-1 mb-3"] do
        L.span_ [L.class_ "text-muted me-1"] "Exercises: "
        L.ul_ do
          forM_ exercises \exercise' -> L.li_ (L.toHtml exercise'.name)
    L.form_ [L.action_ "/repeat-last"] do
      L.button_ [L.class_ "btn btn-primary"] "♻️ Repeat this workout"

viewChooseOuter :: [DBN.Muscle] -> [DBN.Soreness] -> [DBN.ExerciseWithWorkouts] -> L.Html ()
viewChooseOuter allMuscles' currentSoreness currentTraining =
  viewHtmlSkeleton
    PageChooseExercise
    ( viewChoose allMuscles' currentSoreness currentTraining
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

viewStats :: [DBN.Muscle] -> Map.Map DBN.Muscle (Double, Double) -> TL.Text -> L.Html ()
viewStats muscles muscleToRegression overall =
  viewHtmlSkeleton
    PageStats
    do
      L.div_ [L.class_ "row text-center"] do
        L.h5_ "Trends"
        L.table_ [L.class_ "table tbl-sm"] $
          forM_ (chunksOf 2 (sortOn (snd . snd) (Map.toList muscleToRegression))) \musclesAndRegressions -> do
            L.tr_ do
              forM_ musclesAndRegressions \(muscle, (_, slope)) -> do
                L.td_ do
                  L.span_ [L.class_ "me-2"] (L.toHtml muscle.name)
                  let clamp v lo hi = max lo (min hi v)
                      rotationForSlope s = (clamp (-s) (-4) 4 / 4.0) * 45.0
                  when (not (isNaN slope)) do
                    L.i_
                      [ L.style_ ("transform: rotate(" <> packShow (rotationForSlope slope) <> "deg)"),
                        L.style_ "display: inline-block",
                        L.class_ (if slope < -0.5 then "text-danger" else "")
                      ]
                      "➡️"
                  L.span_ [L.class_ "form-text"] (L.toHtml (" (" <> pack (printf "%.2f" slope) <> ")"))

        L.h5_ "Overall"
        L.toHtmlRaw overall
      forM_ (chunksOf 2 muscles) \muscleRow -> do
        L.div_ [L.class_ "row text-center"] do
          forM_ muscleRow \muscle -> do
            L.div_ [L.class_ "col-lg-6 col-12"] do
              L.h5_ (L.toHtml muscle.name)
              L.p_ [L.class_ "form-text"] do
                L.small_ do
                  L.toHtml ("ID " <> packShow muscle.id)
              L.img_ [L.src_ ("/stats/" <> packShow muscle.id), L.width_ "400", L.height_ "300"]
