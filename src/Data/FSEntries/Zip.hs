-- | Functions for zipping two 'FSEntries' together.
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Data.FSEntries.Zip
  ( zipFSEntriesWithA
  ) where

import Data.FSEntries.Types
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import qualified Data.Set as S

-- | Combine two 'FSEntries' values using the given function to merge
-- 'FSEntry's at the same place in the hierarchy.  Since two
-- directories may not both have entries at the same names, the merge
-- function takes optional arguments.  Since errors may arise, the
-- function runs in an 'Applicative'.
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

{-
-- This needs to carry location information, so I'll leave it
-- commented out for later.
overwrite
  :: (Applicative m, Monoid d)
  => Maybe (FSEntry d f) -> Maybe (FSEntry d f) -> m (Maybe (FSEntry d f))
overwrite Nothing mEntry = pure mEntry
overwrite mEntry Nothing = pure mEntry
overwrite (Just (Dir d entries)) (Just (Dir d' entries')) =
  Just . Dir (d <> d') <$> zipFSEntriesWithA overwrite entries entries'
overwrite (Just (File _)) f@(Just (File _)) = pure f
overwrite (Just (Dir _ _)) (Just (File _)) = error "dir/file conflict"
overwrite (Just (File _)) (Just (Dir _ _)) = error "file/dir conflict"
-}
