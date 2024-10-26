{-# LANGUAGE BlockArguments #-}

module Main (main) where

import Control.Monad (mapM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bool (Bool (True))
import Data.Maybe (Maybe (Nothing))
import Database.SQLite.Simple (execute, execute_, withConnection)
import Myocardio.DatabaseNew (retrieveExercises, withDatabase)
import System.IO (IO, print, putStrLn)

main :: IO ()
main = withDatabase \connection -> do
  putStrLn "migration finished"
  exs <- retrieveExercises connection True
  mapM_ print exs
