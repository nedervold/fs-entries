-- | Hedgehog generators for 'FSEntry' and 'FSEntries'.
module Data.FSEntries.Generators
  ( genFSEntry
  , genFSEntries
  ) where

import Data.FSEntries.Types
import qualified Data.Map as M
import Hedgehog
import Hedgehog.Gen
import qualified Hedgehog.Range as Rng

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
  pairs <- list (Rng.linear 0 5) genPair
  return $ FSEntries $ M.fromList pairs
  where
    genPair = do
      fileName <- genFileName
      entry <- genFSEntry gd gf
      return (fileName, entry)

genFileName
  :: MonadGen m
  => m String
genFileName = string (Rng.constant 1 5) lower
