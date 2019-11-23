-- | Functions for zipping two 'FSEntries' together.
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module Data.FSEntries.Zip where

import Control.Applicative (liftA2)
import Data.FSEntries.Functor
import Data.FSEntries.Types
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Validation
import Safe

-- | Merge optional 'FSEntry's.  They are optional because when
-- merging two FSEntries, there may or may not be an 'FSEntry' at any
-- given name.  The result is an 'Applicative' value to allow for
-- errors.  Must not return 'Nothing' except when both arguments are
-- 'Nothing'.
type MergeFunc f' d f = Maybe (FSEntryF f' d f) -> Maybe (FSEntryF f' d f) -> f' (Maybe (FSEntryF f' d f))

interleaveMergeFunc :: MergeFunc (Validation ()) d f
interleaveMergeFunc Nothing rhs = pure rhs
interleaveMergeFunc lhs Nothing = pure lhs
interleaveMergeFunc _ _ = Failure ()

overrideMergeFunc :: MergeFunc (Validation ()) () f
overrideMergeFunc Nothing rhs = pure rhs
overrideMergeFunc lhs Nothing = pure lhs
overrideMergeFunc (Just (FileF _)) f@(Just (FileF _)) = pure f
-- TODO Implement.
overrideMergeFunc (Just (DirF () d)) (Just (DirF () d')) =
  error "must merge contents"
overrideMergeFunc _ _ = Failure ()

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
        h lhs rhs = fromJust <$> join' (liftA2 mergeEntriesF lhs rhs)
        fromJust :: Maybe a -> a
        fromJust =
          fromJustNote
            "concatZip: mergeEntriesF returned Nothing with non-Nothing arguments"
{-
-- | Lift a function that applicatively merges (possibly missing)
-- 'FSEntry' values into a function that applicatively merges
-- 'FSEntries'.  Since two directories may not both have entries
-- present at the same names, we work with 'Maybe' 'FSEntry'
-- values. The functions run in an 'Applicative' context to allow for
-- errors.
zipFSEntriesWithA
  :: forall d d' d'' f f' f'' m.
     Applicative m
  => (Maybe (FSEntry d f) -> Maybe (FSEntry d' f') -> m (Maybe (FSEntry d'' f'')))
  -> FSEntries d f
  -> FSEntries d' f'
  -> m (FSEntries d'' f'')
zipFSEntriesWithA zipFSEntry (FSEntries m) (FSEntries m') =
  mkFSEntries <$> mapMaybeA keyToPair keys
  where
    keyToPair :: String -> m (Maybe (String, FSEntry d'' f''))
    keyToPair key =
      fmap (key, ) <$> zipFSEntry (M.lookup key m) (M.lookup key m')
    keys :: [String]
    keys = S.toList $ M.keysSet m `S.union` M.keysSet m'

mapMaybeA
  :: Applicative m
  => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeA f = fmap catMaybes . traverse f

------------------------------------------------------------}
