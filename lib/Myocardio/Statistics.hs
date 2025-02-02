module Myocardio.Statistics (viewChartForWorkouts, weekToCountMap, histogramForWorkouts, regressionForWorkouts) where

import Control.Applicative (pure)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifunctor (second)
import Data.Bool ((&&))
import Data.ByteString.Lazy qualified as BSL
import Data.Colour.SRGB (sRGB24read)
import Data.Eq ((/=))
import Data.Foldable (Foldable, foldMap)
import Data.Function (($), (&), (.))
import Data.Functor (fmap, (<$>))
import Data.Int (Int)
import Data.List (filter)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (Maybe (Just, Nothing))
import Data.Monoid (Sum (Sum, getSum), mempty, (<>))
import Data.MonoidMap qualified as MonoidMap
import Data.Ord ((<))
import Data.Tuple (fst, snd)
import Data.Vector.Unboxed qualified as V
import Graphics.Rendering.Chart (laxis_generate, layout_y_axis, makeAxis, plotBars)
import Graphics.Rendering.Chart.Backend.Cairo (FileFormat (PNG), toFile, _fo_format, _fo_size)
import Graphics.Rendering.Chart.Easy (bars, def, fill_color, layout_x_axis, opaque, plot_bars_item_styles, (.=), (.~))
import Graphics.Rendering.Chart.State (plot)
import Myocardio.AbsoluteWeek (AbsoluteWeek, absoluteWeekToInt)
import Myocardio.DatabaseNew (MuscleWithWorkoutWeek (week))
import Statistics.LinearRegression qualified as LR
import Text.Show (show)
import UnliftIO (withSystemTempDirectory)
import Prelude (Double, fromIntegral, (+), (-))

weekToCountMap :: (Foldable f) => f AbsoluteWeek -> MonoidMap.MonoidMap AbsoluteWeek (Sum Int)
weekToCountMap = foldMap (`MonoidMap.singleton` Sum 1)

toExtendedList :: (AbsoluteWeek, AbsoluteWeek) -> MonoidMap.MonoidMap AbsoluteWeek (Sum Int) -> [(AbsoluteWeek, Int)]
toExtendedList (fromWeek, toWeek) l =
  let mapAsList :: [(AbsoluteWeek, Int)]
      mapAsList = second getSum <$> MonoidMap.toList l
   in case NE.nonEmpty mapAsList of
        Nothing -> []
        Just ne ->
          let minElement = fst (NE.head ne)
              maxElement = fst (NE.last ne)
           in [(e, 0) | e <- [fromWeek .. (minElement - 1)]]
                <> mapAsList
                <> [(e, 0) | e <- [(maxElement + 1) .. toWeek]]

histogramForWorkouts :: (AbsoluteWeek, AbsoluteWeek) -> [MuscleWithWorkoutWeek] -> [(AbsoluteWeek, Int)]
histogramForWorkouts range musclesWithDate =
  toExtendedList range $ weekToCountMap ((.week) <$> musclesWithDate)

viewChartForWorkouts :: (MonadIO m) => (AbsoluteWeek, AbsoluteWeek) -> [MuscleWithWorkoutWeek] -> m BSL.ByteString
viewChartForWorkouts range musclesWithDate =
  case musclesWithDate of
    [] -> pure mempty
    myMuscle -> liftIO do
      let weekHistogram :: [(AbsoluteWeek, Int)]
          weekHistogram = histogramForWorkouts range myMuscle
      withSystemTempDirectory "chart" \tempDirPath -> do
        let filePath = tempDirPath <> "/chart.png"
        toFile (def {_fo_format = PNG, _fo_size = (400, 300)}) filePath do
          layout_y_axis . laxis_generate .= (\_ -> makeAxis (fmap show) ([0, 1, 2, 3, 4, 5, 6, 7], [], [0, 1, 2, 3, 4, 5, 6, 7]))
          layout_x_axis . laxis_generate .= (\_ -> makeAxis (fmap show) ([0 .. snd range], [], []))
          let barsDefinition = bars ["Frequency"] [(week, [count]) | (week, count) <- weekHistogram]
              setStyle bs = bs & plot_bars_item_styles .~ [(def & fill_color .~ opaque (sRGB24read "#0a58ca"), Nothing)]
          plot $ plotBars . setStyle <$> barsDefinition
        BSL.readFile filePath

regressionForWorkouts :: [(AbsoluteWeek, Int)] -> (Double, Double)
regressionForWorkouts musclesWithDate =
  let -- lastWeeks = reverse (take 8 (reverse musclesWithDate))
      lastWeeks = musclesWithDate
      input = filter (\(week, count) -> count /= 0 && absoluteWeekToInt week < 55) lastWeeks
   in LR.linearRegression
        (V.fromList ((\(_, count) -> fromIntegral count) <$> input))
        (V.fromList ((\(w, _) -> fromIntegral (absoluteWeekToInt w)) <$> input))
