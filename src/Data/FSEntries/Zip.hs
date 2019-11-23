-- | Functions for zipping two 'FSEntries' together.
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Data.FSEntries.Zip where

import Control.Applicative (liftA2)
import Data.FSEntries.Functor
import Data.FSEntries.Types
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import Data.Validation

-- | Merge optional 'FSEntry's.  They are optional because when
-- merging two FSEntries, there may or may not be an 'FSEntry' at any
-- given name.  The result is an 'Applicative' value to allow for
-- errors.  Will never be called with two 'Nothing' values.
type MergeFunc f' d f = Maybe (FSEntryF f' d f) -> Maybe (FSEntryF f' d f) -> f' (FSEntryF f' d f)

interleaveMergeFunc :: MergeFunc (Validation ()) d f
interleaveMergeFunc Nothing Nothing = error "interleaveMergeFunc: impossible"
interleaveMergeFunc Nothing (Just rhs) = pure rhs
interleaveMergeFunc (Just lhs) Nothing = pure lhs
interleaveMergeFunc _ _ = Failure ()

overrideMergeFunc
  :: forall f.
     MergeFunc (Validation ()) () f
overrideMergeFunc Nothing Nothing = error "interleaveMergeFunc: impossible"
overrideMergeFunc Nothing (Just rhs) = pure rhs
overrideMergeFunc (Just lhs) Nothing = pure lhs
overrideMergeFunc (Just (FileF _)) (Just f@(FileF _)) = pure f
overrideMergeFunc (Just (DirF () entries)) (Just (DirF () entries')) =
  pure $ DirF () $ mergeFSEntriesF joinV overrideMergeFunc entries entries'
overrideMergeFunc _ _ = Failure ()

joinV :: Validation e (Validation e a) -> Validation e a
joinV v =
  case v of
    Failure v' -> Failure v'
    Success v' -> v'

mergeFSEntriesF
  :: forall f' d f.
     Applicative f'
  => (forall a. f' (f' a) -> f' a)
  -> MergeFunc f' d f
  -> FSEntriesF f' d f
  -> FSEntriesF f' d f
  -> FSEntriesF f' d f
mergeFSEntriesF join' mergeEntry (FSEntriesF m) (FSEntriesF m') =
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

concatZip
  :: forall f' d f.
     Applicative f'
  => (forall a. f' (f' a) -> f' a)
  -> MergeFunc f' d f
  -> [FSEntries d f]
  -> f' (FSEntries d f)
concatZip join' mergeEntriesF entriesList = contractEntries join' x
  where
    x :: FSEntriesF f' d f
    x = foldl f emptyFSEntriesF entriesList
    f :: FSEntriesF f' d f -> FSEntries d f -> FSEntriesF f' d f
    f entriesF entries = g entriesF (expandEntries entries)
    g :: FSEntriesF f' d f -> FSEntriesF f' d f -> FSEntriesF f' d f
    g (FSEntriesF m) (FSEntriesF m') = FSEntriesF $ M.fromList x'
      where
        x' :: [(String, f' (FSEntryF f' d f))]
        x' =
          [ (key, h (sequenceA $ M.lookup key m) (sequenceA $ M.lookup key m'))
          | key <- S.toList keys
          ]
        keys :: S.Set String
        keys = M.keysSet m `S.union` M.keysSet m'
        h
          :: f' (Maybe (FSEntryF f' d f))
          -> f' (Maybe (FSEntryF f' d f))
          -> f' (FSEntryF f' d f)
        h lhs rhs = join' (liftA2 mergeEntriesF lhs rhs)

mapMaybeA
  :: Applicative f
  => (a -> f (Maybe b)) -> [a] -> f [b]
mapMaybeA f = fmap catMaybes . traverse f
