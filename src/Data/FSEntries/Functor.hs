-- | Datatypes for filesystem-like hierarchical data.
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Data.FSEntries.Functor
    -- * Datatypes
  ( FSEntriesF(..)
  , FSEntryF(..)
    -- * Construction
  , emptyFSEntriesF
    -- * Conversion
  , expandEntries
  , expandEntry
  , contractEntries
  , contractEntry
  ) where

import Data.FSEntries.Types
import qualified Data.Map as M
import GHC.Generics (Generic)

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

------------------------------------------------------------
-- | Convert an 'FSEntries' into an 'FSEntriesF'.  The functor must be
-- applicative, so we have a 'pure'.
expandEntries
  :: forall d f f'.
     Applicative f'
  => FSEntries d f -> FSEntriesF f' d f
expandEntries entries =
  FSEntriesF $ fmap (pure . expandEntry) $ unFSEntries entries

-- | Convert an 'FSEntry' into an 'FSEntryF'.  The functor must be
-- applicative, so we have a 'pure'.
expandEntry
  :: forall d f f'.
     Applicative f'
  => FSEntry d f -> FSEntryF f' d f
expandEntry (Dir d entries) = DirF d $ expandEntries entries
expandEntry (File f) = FileF f

-- TODO Might be better served as a typeclass.
type JoinFunc f' = forall a. f' (f' a) -> f' a

-- | Convert an 'FSEntriesF' into an applicative value of 'FSEntries'.
-- The applicative gets applied twice, so we need a function to reduce
-- it.
contractEntries
  :: forall f' d f.
     Applicative f'
  => JoinFunc f' -> FSEntriesF f' d f -> f' (FSEntries d f)
contractEntries join' entries = FSEntries <$> join' z
    -- Oops!  I end up with nested f'.  It's not monadic, so I need
    -- some specific knowledge of the applicative to join the two
    -- applications, don't I?  No problem for Validation: it's just a
    -- coercion.
  where
    z :: f' (f' (M.Map String (FSEntry d f)))
    z =
      fmap sequenceA $
      traverse (fmap $ contractEntry join') $ unFSEntriesF entries

-- | Convert an 'FSEntryF' into an applicative value of 'FSEntry'.
-- The applicative gets applied twice, so we need a function to reduce
-- it.
contractEntry
  :: Applicative f'
  => JoinFunc f' -> FSEntryF f' d f -> f' (FSEntry d f)
contractEntry join' (DirF d entries) =
  Dir d <$> contractEntries join' entries
contractEntry _ (FileF f) = pure $ File f
