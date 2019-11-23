-- | Functions for zipping two 'FSEntries' together.
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Data.FSEntries.Zip
  ( interleave
  , override
    -- * building blocks
  , MergeFunc
  , mergeAllFSEntries
  ) where

import Control.Applicative (liftA2)
import Data.FSEntries.Functor
import Data.FSEntries.Joinable
import Data.FSEntries.Types
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Validation (Validation(..))

-- TODO Perhaps MergeFunc could be better implemented with a datatype
-- to remove the impossible state: there can be exactly one or two
-- FSEntryFs. Do we need the one-FSEntryF case?  In all the versions I
-- can think of, we just use it if it's a singleton.  Ponder.

-- | Merge optional 'FSEntry's.  They are optional because when
-- merging two FSEntries, there may or may not be an 'FSEntry' at any
-- given name.  The result is an 'Applicative' value to allow for
-- errors.  Will never be called with two 'Nothing' values.
type MergeFunc f' d f = Maybe (FSEntryF f' d f) -> Maybe (FSEntryF f' d f) -> f' (FSEntryF f' d f)

type V = Validation ()

interleaveMergeFunc :: MergeFunc V d f
interleaveMergeFunc Nothing Nothing = error "interleaveMergeFunc: impossible"
interleaveMergeFunc Nothing (Just rhs) = pure rhs
interleaveMergeFunc (Just lhs) Nothing = pure lhs
interleaveMergeFunc _ _ = Failure ()

overrideMergeFunc
  :: forall f.
     MergeFunc V () f
overrideMergeFunc Nothing Nothing = error "overrideMergeFunc: impossible"
overrideMergeFunc Nothing (Just rhs) = pure rhs
overrideMergeFunc (Just lhs) Nothing = pure lhs
overrideMergeFunc (Just (FileF _)) (Just f@(FileF _)) = pure f
overrideMergeFunc (Just (DirF () entries)) (Just (DirF () entries')) =
  pure $ DirF () $ mergeFSEntriesF overrideMergeFunc entries entries'
overrideMergeFunc _ _ = Failure ()

mergeFSEntriesF
  :: forall f' d f.
     Joinable f'
  => MergeFunc f' d f
  -> FSEntriesF f' d f
  -> FSEntriesF f' d f
  -> FSEntriesF f' d f
mergeFSEntriesF mergeEntry (FSEntriesF m) (FSEntriesF m') =
  FSEntriesF $ M.fromList [(key, mergeEntryFor key) | key <- S.toList keys]
  where
    mergeEntryFor :: String -> f' (FSEntryF f' d f)
    mergeEntryFor key = join' $ liftA2 mergeEntry mV mV'
      where
        mV, mV' :: f' (Maybe (FSEntryF f' d f))
        mV = sequenceA $ M.lookup key m
        mV' = sequenceA $ M.lookup key m'
    keys :: S.Set String
    keys = M.keysSet m `S.union` M.keysSet m

mergeAllFSEntries
  :: forall f' d f.
     Joinable f'
  => MergeFunc f' d f -> [FSEntries d f] -> f' (FSEntries d f)
mergeAllFSEntries mergeEntriesF entriesList =
  contractEntries $ foldl mergeMixedEntries emptyFSEntriesF entriesList
  where
    mergeMixedEntries :: FSEntriesF f' d f -> FSEntries d f -> FSEntriesF f' d f
    mergeMixedEntries entriesF entries =
      mergeFSEntriesF mergeEntriesF entriesF (expandEntries entries)

interleave :: [FSEntries d f] -> V (FSEntries d f)
interleave = mergeAllFSEntries interleaveMergeFunc

override :: [FSEntries () f] -> V (FSEntries () f)
override = mergeAllFSEntries overrideMergeFunc
