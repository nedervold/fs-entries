-- | Datatypes for filesystem-like hierarchical data.
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.FSEntries.Functor
  ( -- * Datatypes
    FSEntriesF(..)
  , FSEntryF(..)
    -- * Construction
  , emptyFSEntriesF
    -- * Conversion
  , expandEntries
  , expandEntry
  , contractEntries
  , contractEntry
  ) where

import Data.FSEntries.Joinable (Joinable(..))
import Data.FSEntries.Types
import qualified Data.Map as M
import Data.Validation (Validation)
import GHC.Generics (Generic)
import Text.Printf (printf)

type V = Validation ()

-- | A datatype representing the contents of a directory.  Files and
-- directories may contain arbitrary data.  'FSEntriesF' allows
-- weaving a functor into the structure.  In 'FSEntries', the functor
-- is left out.
newtype FSEntriesF f' d f = FSEntriesF
  { unFSEntriesF :: M.Map String (f' (FSEntryF f' d f))
  } deriving (Generic)

-- | An empty 'FSEntriesF' value.
emptyFSEntriesF :: FSEntriesF f' d f
emptyFSEntriesF = FSEntriesF M.empty

-- | A datatype representing an element of the contents of a
-- directory.  'FSEntryF' allows weaving a functor into the structure.
-- In 'FSEntry', the functor is left out.
data FSEntryF f' d f
  = DirF d
         (FSEntriesF f' d f) -- ^ represents a directory
  | FileF f -- ^ represents a file
  deriving (Generic)

instance (Eq d, Eq f) => Eq (FSEntriesF V d f) where
  FSEntriesF m == FSEntriesF m' = m == m'

instance (Eq d, Eq f) => Eq (FSEntryF V d f) where
  FileF f == FileF f' = f == f'
  DirF d entries == DirF d' entries' = d == d' && entries == entries'
  _ == _ = False

instance Show (FSEntryF V () String) where
  show (FileF f) = printf "FileF %s" (show f)
  show (DirF d entries) = printf "DirF %s %s" (show d) (show entries)

instance Show (FSEntriesF V () String) where
  show (FSEntriesF m) = printf "FSEntries %s" (show m)

------------------------------------------------------------
-- | Convert an 'FSEntries' into an 'FSEntriesF'.  The functor must be
-- applicative, so we have a 'pure'.
expandEntries ::
     forall d f f'. Applicative f'
  => FSEntries d f
  -> FSEntriesF f' d f
expandEntries entries =
  FSEntriesF $ pure . expandEntry <$> unFSEntries entries

-- | Convert an 'FSEntry' into an 'FSEntryF'.  The functor must be
-- applicative, so we have a 'pure'.
expandEntry ::
     forall d f f'. Applicative f'
  => FSEntry d f
  -> FSEntryF f' d f
expandEntry (Dir d entries) = DirF d $ expandEntries entries
expandEntry (File f) = FileF f

-- | Convert an 'FSEntriesF' into an applicative value of 'FSEntries'.
-- The applicative gets applied twice, so we need a function to reduce
-- it.
contractEntries ::
     forall f' d f. Joinable f'
  => FSEntriesF f' d f
  -> f' (FSEntries d f)
contractEntries entries = FSEntries <$> join' z
  where
    z :: f' (f' (M.Map String (FSEntry d f)))
    z =
      fmap sequenceA $ traverse (fmap contractEntry) $ unFSEntriesF entries

-- | Convert an 'FSEntryF' into an applicative value of 'FSEntry'.
-- The applicative gets applied twice, so we need a function to reduce
-- it.
contractEntry :: Joinable f' => FSEntryF f' d f -> f' (FSEntry d f)
contractEntry (DirF d entries) = Dir d <$> contractEntries entries
contractEntry (FileF f) = pure $ File f
