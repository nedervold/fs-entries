-- | Datatypes for filesystem-like hierarchical data.
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}

module Data.FSEntries.Types
  ( -- * Datatypes
    FSEntries(..)
  , FSEntry(..)
    -- * Convenience functions
  , mkFSEntries
  , mkDir
  , mkFile
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
  } deriving (Eq, Ord, Foldable, Generic, Show, Traversable)

-- | A datatype representing an element of the contents of a
-- directory.
data FSEntry d f
  = Dir d
        (FSEntries d f) -- ^ represents a directory
  | File f -- ^ represents a file
  deriving (Eq, Ord, Foldable, Generic, Show, Traversable)

instance (NFData d, NFData f) =>
         NFData (FSEntries d f)

instance (NFData d, NFData f) =>
         NFData (FSEntry d f)

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
instance Functor (FSEntries d) where
  fmap = bimap id

instance Bifunctor FSEntries where
  bimap d f entries = FSEntries $ fmap (bimap d f) (unFSEntries entries)

instance Bifoldable FSEntries where
  bifoldMap f g entries =
    mconcat [bifoldMap f g v | (_k, v) <- M.toList $ unFSEntries entries]

instance Bitraversable FSEntries where
  bitraverse f g entries =
    FSEntries <$> traverse (bitraverse f g) (unFSEntries entries)

------------------------------------------------------------
instance Functor (FSEntry d) where
  fmap = bimap id

instance Bifunctor FSEntry where
  bimap d f (Dir d' entries) = Dir (d d') (bimap d f entries)
  bimap _d f (File f') = File (f f')

instance Bifoldable FSEntry where
  bifoldMap _f g (File f') = g f'
  bifoldMap f g (Dir d entries) = f d <> bifoldMap f g entries

instance Bitraversable FSEntry where
  bitraverse f g (Dir d entries) = Dir <$> f d <*> bitraverse f g entries
  bitraverse _f g (File f') = File <$> g f'
