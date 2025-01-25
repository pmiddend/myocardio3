module Myocardio.AbsoluteWeek
  ( AbsoluteWeek,
    absoluteWeekFromRelative,
    getCurrentAbsoluteWeek,
    weekToString,
    beginningOfYear,
    absoluteWeekToInt,
    beginningOfAbsoluteWeeks,
  )
where

import Control.Applicative (pure)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Eq (Eq)
import Data.Int (Int)
import Data.Ord (Ord)
import Data.String (String)
import Data.Time (DayOfWeek (Monday), utctDay)
import Data.Time.Calendar.WeekDate (FirstWeekType (FirstMostWeek), toWeekCalendar)
import Data.Time.Clock (getCurrentTime)
import Graphics.Rendering.Chart (PlotValue)
import Graphics.Rendering.Chart.Axis.Types (PlotValue (autoAxis, fromValue, toValue))
import Graphics.Rendering.Chart.Easy (autoScaledIntAxis, defaultIntAxis)
import Text.Show (Show, show)
import Prelude (Enum, Integral, Num, Real, fromIntegral, round, (*), (+), (-))

newtype AbsoluteWeek = AbsoluteWeek Int deriving (Ord, Eq, Real, Enum, Num, Integral)

instance Show AbsoluteWeek where
  show (AbsoluteWeek w) = show w

beginningOfAbsoluteWeeks :: AbsoluteWeek
beginningOfAbsoluteWeeks = beginningOfYear 2024

beginningOfYear :: Int -> AbsoluteWeek
beginningOfYear year = AbsoluteWeek (linearizeYearAndWeek year (0 :: Int))

absoluteWeekToInt :: AbsoluteWeek -> Int
absoluteWeekToInt (AbsoluteWeek w) = w

instance PlotValue AbsoluteWeek where
  toValue (AbsoluteWeek i) = fromIntegral i
  fromValue d = AbsoluteWeek (round d)
  autoAxis = autoScaledIntAxis defaultIntAxis

weekToString :: AbsoluteWeek -> String
weekToString (AbsoluteWeek i) = show i

linearizeYearAndWeek :: (Integral a1, Integral a2, Num a3) => a1 -> a2 -> a3
linearizeYearAndWeek year week = (fromIntegral year - 2024) * 52 + fromIntegral week

absoluteWeekFromRelative :: Int -> Int -> AbsoluteWeek
absoluteWeekFromRelative year week = AbsoluteWeek (linearizeYearAndWeek year week)

getCurrentAbsoluteWeek :: (MonadIO m) => m AbsoluteWeek
getCurrentAbsoluteWeek = do
  currentTime <- liftIO getCurrentTime
  let (currentYear, currentWeek, _) = toWeekCalendar FirstMostWeek Monday (utctDay currentTime)
  pure (AbsoluteWeek (linearizeYearAndWeek currentYear currentWeek))
