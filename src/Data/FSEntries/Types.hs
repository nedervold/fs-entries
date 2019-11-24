-- | Datatypes for filesystem-like hierarchical data.
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.FSEntries.Types
  ( -- * Datatypes
    FSEntries(..)
  , FSEntry(..)
    -- * Construction
  , emptyFSEntries
  , mkFSEntries
  , mkDir
  , mkFile
    -- * Deletion
  , pruneEmptyDirs
  ) where

import Control.DeepSeq (NFData(..))
import Data.Bifoldable (Bifoldable(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Bitraversable (Bitraversable(..))
import qualified Data.Map as M
import GHC.Generics (Generic)

-- | A datatype representing the contents of a directory.  Files and
-- directories may contain arbitrary data.
newtype FSEntries d f = FSEntries
  { unFSEntries :: M.Map String (FSEntry d f)
  } deriving (Eq, Ord, Functor, Foldable, Traversable, Generic, Show)

-- | An empty 'FSEntries' value.
emptyFSEntries :: FSEntries d f
emptyFSEntries = FSEntries M.empty

-- | A datatype representing an element of the contents of a
-- directory.
data FSEntry d f
  = Dir d
        (FSEntries d f) -- ^ represents a directory
  | File f -- ^ represents a file
  deriving (Eq, Ord, Functor, Foldable, Traversable, Generic, Show)

------------------------------------------------------------
instance (NFData d, NFData f) => NFData (FSEntries d f)

instance (NFData d, NFData f) => NFData (FSEntry d f)

------------------------------------------------------------
-- | A convenience function for creating 'FSEntries'.
mkFSEntries :: [(String, FSEntry d f)] -> FSEntries d f
mkFSEntries = FSEntries . M.fromList

-- | A convenience function for creating a named directory 'FSEntry'.
-- Intended to be used as an argument to 'mkFSEntries' or 'mkDir'.
mkDir :: String -> d -> [(String, FSEntry d f)] -> (String, FSEntry d f)
mkDir fileName d entries = (fileName, Dir d $ mkFSEntries entries)

-- | A convenience function for creating a file 'FSEntry' with name.
-- Intended to be used as an argument to 'mkFSEntries' or 'mkDir'.
mkFile :: String -> f -> (String, FSEntry d f)
mkFile fileName f = (fileName, File f)

------------------------------------------------------------
-- | Recursively prune empty directories.
pruneEmptyDirs :: FSEntries d f -> FSEntries d f
pruneEmptyDirs (FSEntries entries) =
  FSEntries $ M.filter (not . isEmptyDir) $ fmap pruneEmptyDirs' entries
  where
    isEmptyDir :: FSEntry d f -> Bool
    isEmptyDir (Dir _ entries') = M.null $ unFSEntries entries'
    isEmptyDir _ = True
    pruneEmptyDirs' :: FSEntry d f -> FSEntry d f
    pruneEmptyDirs' f@(File _) = f
    pruneEmptyDirs' (Dir d entries') = Dir d $ pruneEmptyDirs entries'

------------------------------------------------------------
instance Bifunctor FSEntries where
  bimap f g entries = FSEntries $ bimap f g <$> unFSEntries entries

instance Bifunctor FSEntry where
  bimap f g (Dir d entries) = Dir (f d) $ bimap f g entries
  bimap _f g (File f') = File $ g f'

------------------------------------------------------------
instance Bifoldable FSEntries where
  bifoldMap f g entries =
    mconcat [bifoldMap f g v | (_k, v) <- M.toList $ unFSEntries entries]

instance Bifoldable FSEntry where
  bifoldMap _f g (File f') = g f'
  bifoldMap f g (Dir d entries) = f d <> bifoldMap f g entries

------------------------------------------------------------
instance Bitraversable FSEntries where
  bitraverse f g entries =
    FSEntries <$> traverse (bitraverse f g) (unFSEntries entries)

instance Bitraversable FSEntry where
  bitraverse f g (Dir d entries) = Dir <$> f d <*> bitraverse f g entries
  bitraverse _f g (File f') = File <$> g f'
