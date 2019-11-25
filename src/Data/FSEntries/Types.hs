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
  , (<//>)
  , nest
  , nest1
    -- * Conversion
  , toDirList
  , toFileList
  , toEntryList
    -- * Transformation
  , tupleWithPath
    -- * Traverse
  , indexTraverse
  , indexBitraverse
    -- * Deletion
  , pruneEmptyDirs
  ) where

import Control.DeepSeq (NFData(..))
import Control.Monad.Reader (Reader, ask, local, runReader)
import Data.Bifoldable (Bifoldable(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Bitraversable (Bitraversable(..))
import Data.Foldable (Foldable(..))
import qualified Data.Map as M
import qualified Data.Set as S
import GHC.Generics (Generic)
import System.FilePath ((</>), splitDirectories)

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
-- | Place the 'FSEntries' in a named directory.
nest1 :: String -> FSEntries () f -> FSEntries () f
nest1 dirNm entries = FSEntries $ M.fromList [(dirNm, Dir () entries)]

-- | Place the 'FSEntries' in a directory at the 'FilePath'.  Leading
-- and trailing slashes are ignored.
nest :: FilePath -> FSEntries () f -> FSEntries () f
nest fp entries =
  foldr nest1 entries $
  case splitDirectories fp of
    "/":rst -> rst
    rst -> rst

-- | Place the 'FSEntries' in a directory at the 'FilePath'.  Leading
-- and trailing slashes are ignored.  A synonym for 'nest'.
(<//>) :: FilePath -> FSEntries () f -> FSEntries () f
(<//>) = nest

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
-- | For each piece of directory or file data, tuple it up with its
-- 'FilePath' in the 'FSEntries'.
tupleWithPath ::
     forall d f. FSEntries d f -> FSEntries (FilePath, d) (FilePath, f)
tupleWithPath entries = runReader (doEntries entries) ""
  where
    doEntries ::
         FSEntries d f
      -> Reader FilePath (FSEntries (FilePath, d) (FilePath, f))
    doEntries (FSEntries m) = FSEntries . M.fromList <$> mapM doKey keys
      where
        keys = S.toList $ M.keysSet m
        doKey ::
             String
          -> Reader FilePath (String, FSEntry (FilePath, d) (FilePath, f))
        doKey key =
          local (</> key) $ do
            let entry = m M.! key
            entry' <- doEntry entry
            pure (key, entry')
    doEntry ::
         FSEntry d f -> Reader FilePath (FSEntry (FilePath, d) (FilePath, f))
    doEntry (Dir d entries') = do
      fp <- ask
      Dir (fp, d) <$> doEntries entries'
    doEntry (File f) = do
      fp <- ask
      pure $ File (fp, f)

-- | Traverse files in the 'FSEntries' with access to their 'FilePath's.
indexTraverse ::
     Applicative m
  => (FilePath -> f -> m f')
  -> FSEntries d f
  -> m (FSEntries d f')
indexTraverse f entries =
  bitraverse (pure . snd) (uncurry f) $ tupleWithPath entries

-- | Traverse directories and files in the 'FSEntries' with access to
-- their 'FilePath's.
indexBitraverse ::
     Applicative m
  => (FilePath -> d -> m d')
  -> (FilePath -> f -> m f')
  -> FSEntries d f
  -> m (FSEntries d' f')
indexBitraverse f g entries =
  bitraverse (uncurry f) (uncurry g) $ tupleWithPath entries

------------------------------------------------------------
toFileList :: FSEntries d f -> [(FilePath, f)]
toFileList entries = toList $ tupleWithPath entries

toEntryList :: FSEntries d f -> [(FilePath, Either d f)]
toEntryList entries =
  bifoldMap
    (\(fp, d) -> ((fp, Left d) :))
    (\(fp, f) -> ((fp, Right f) :))
    (tupleWithPath entries)
    []

toDirList :: FSEntries d f -> [(FilePath, d)]
toDirList entries =
  bifoldMap (\(fp, d) -> ((fp, d) :)) (const id) (tupleWithPath entries) []

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
