-- | Hedgehog generators for 'FSEntry' and 'FSEntries'.
module Data.FSEntries.Generators
  ( genFSEntry
  , genFSEntries
  ) where

import Data.FSEntries.Types
import qualified Data.Map as M
import Hedgehog (MonadGen)
import Hedgehog.Gen (choice, list, lower, recursive, string)
import Hedgehog.Range (constant, linear)

-- TODO This works, but I don't understand ranges properly.  Figure
-- them out, then fix this.
-- | Given generators for the directory and file data, a generator for
-- an 'FSEntry'.
genFSEntry
  :: MonadGen m
  => m d -> m f -> m (FSEntry d f)
genFSEntry gd gf = recursive choice [genFile] [genDir]
  where
    genFile = do
      f <- gf
      pure $ File f
    genDir = do
      d <- gd
      Dir d <$> genFSEntries gd gf

-- | Given generators for the directory and file data, a generator for
-- an 'FSEntries'.
genFSEntries
  :: MonadGen m
  => m d -> m f -> m (FSEntries d f)
genFSEntries gd gf = do
  pairs <- list (linear 0 5) genPair
  return $ FSEntries $ M.fromList pairs
  where
    genPair = do
      fileName <- genFileName
      entry <- genFSEntry gd gf
      return (fileName, entry)

-- | Generate a short filename.
genFileName
  :: MonadGen m
  => m String
genFileName = string (constant 1 8) lower
