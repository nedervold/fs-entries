-- | Datatypes for filesystem-like hierarchical data.
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Data.FSEntries.Types
  ( -- * Datatypes
    FSEntriesF(..)
  , FSEntryF(..)
  , FSEntries
  , FSEntry
  , HKD
    -- * Convenience functions
  , mkFSEntries
  , mkDir
  , mkFile
    -- * Example functions
  , insertMaybe
  ) where

import Control.DeepSeq (NFData(..))
import Control.Monad.Identity (Identity(..))
import Data.Bifoldable (Bifoldable(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Bitraversable (Bitraversable(..))
import qualified Data.Map as M
import GHC.Generics (Generic)

type family HKD f t where
  HKD Identity t = t
  HKD f t = f t

-- | A datatype representing the contents of a directory.  Files and
-- directories may contain arbitrary data.  'FSEntriesF' allows
-- weaving a functor into the structure.  In 'FSEntries', the functor
-- is left out.
newtype FSEntriesF f' d f = FSEntries
  { unFSEntries :: M.Map String (HKD f' (FSEntryF f' d f))
  } deriving (Generic)

-- | A datatype representing an element of the contents of a
-- directory.  'FSEntryF' allows weaving a functor into the structure.
-- In 'FSEntry', the functor is left out.
data FSEntryF f' d f
  = Dir d
        (FSEntriesF f' d f) -- ^ represents a directory
  | File f -- ^ represents a file
  deriving (Generic)

-- | A type representing the contents of a directory.  Files and
-- directories may contain arbitrary data.
type FSEntries = FSEntriesF Identity

-- | A type representing an element of the contents of a directory.
type FSEntry = FSEntryF Identity

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
-- | An example of weaving an applicative functor into an 'FSEntries'
-- value.
insertMaybe
  :: forall d f.
     FSEntries d f -> FSEntriesF Maybe d f
insertMaybe entries = FSEntries $ fmap insertMaybe' $ unFSEntries entries
  where
    insertMaybe' :: FSEntry d f -> HKD Maybe (FSEntryF Maybe d f)
    insertMaybe' (Dir d entries') = pure $ Dir d $ insertMaybe entries'
    insertMaybe' (File f) = pure $ File f

------------------------------------------------------------
-- The use of HKD in FSEntries' complicates things enough that the
-- compiler can no longer automatically generate many of the typeclass
-- instances, so below lie a large number of instance definitions.
------------------------------------------------------------
deriving instance (Eq d, Eq f) => Eq (FSEntries d f)

deriving instance (Eq d, Eq f) => Eq (FSEntry d f)

------------------------------------------------------------
deriving instance (Ord d, Ord f) => Ord (FSEntries d f)

deriving instance (Ord d, Ord f) => Ord (FSEntry d f)

------------------------------------------------------------
instance (NFData d, NFData f) =>
         NFData (FSEntries d f)

instance (NFData d, NFData f) =>
         NFData (FSEntry d f)

------------------------------------------------------------
deriving instance (Show d, Show f) => Show (FSEntries d f)

deriving instance (Show d, Show f) => Show (FSEntry d f)

------------------------------------------------------------
instance Functor (FSEntries d) where
  fmap f entries = FSEntries $ fmap (fmap f) $ unFSEntries entries

instance Functor (FSEntryF Identity d) where
  fmap f (Dir d entries) = Dir d $ fmap f entries
  fmap f (File f') = File $ f f'

------------------------------------------------------------
instance Bifunctor FSEntries where
  bimap f g entries = FSEntries $ fmap (bimap f g) $ unFSEntries entries

instance Bifunctor FSEntry where
  bimap f g (Dir d entries) = Dir (f d) $ bimap f g entries
  bimap _f g (File f') = File $ g f'

------------------------------------------------------------
instance Foldable (FSEntries d) where
  foldMap f entries = foldMap (foldMap f) $ unFSEntries entries

instance Foldable (FSEntry d) where
  foldMap f (Dir _ entries) = foldMap f entries
  foldMap f (File f') = f f'

------------------------------------------------------------
instance Bifoldable FSEntries where
  bifoldMap f g entries =
    mconcat [bifoldMap f g v | (_k, v) <- M.toList $ unFSEntries entries]

instance Bifoldable FSEntry where
  bifoldMap _f g (File f') = g f'
  bifoldMap f g (Dir d entries) = f d <> bifoldMap f g entries

------------------------------------------------------------
instance Traversable (FSEntries d) where
  traverse f entries =
    FSEntries <$> traverse (traverse f) (unFSEntries entries)

instance Traversable (FSEntry d) where
  traverse f (File f') = File <$> f f'
  traverse f (Dir d entries) = Dir d <$> traverse f entries

------------------------------------------------------------
instance Bitraversable FSEntries where
  bitraverse f g entries =
    FSEntries <$> traverse (bitraverse f g) (unFSEntries entries)

instance Bitraversable (FSEntryF Identity) where
  bitraverse f g (Dir d entries) = Dir <$> f d <*> bitraverse f g entries
  bitraverse _f g (File f') = File <$> g f'
