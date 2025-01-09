module Myocardio.MapUtils
  ( insertMMap,
    insertMSet,
    multiInsert,
    multiInsertMMap,
  )
where

import Data.IORef (IORef, modifyIORef)
import Data.Map qualified as Map
import Data.Maybe (Maybe (Just, Nothing))
import Data.Ord (Ord)
import Data.Set qualified as Set
import System.IO (IO)
import Prelude ()

insertMMap :: (Ord k) => IORef (Map.Map k a) -> k -> a -> IO ()
insertMMap ref key value = modifyIORef ref (Map.insert key value)

insertMSet :: (Ord a) => IORef (Set.Set a) -> a -> IO ()
insertMSet ref value = modifyIORef ref (Set.insert value)

multiInsert :: (Ord k, Ord p) => k -> p -> Map.Map k (Set.Set p) -> Map.Map k (Set.Set p)
multiInsert key newValue = Map.alter alterer key
  where
    alterer Nothing = Just (Set.singleton newValue)
    alterer (Just oldSet) = Just (Set.insert newValue oldSet)

multiInsertMMap :: (Ord k, Ord p) => IORef (Map.Map k (Set.Set p)) -> k -> p -> IO ()
multiInsertMMap ref key newValue = modifyIORef ref (multiInsert key newValue)
