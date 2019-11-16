module Data.FSEntries.IOSpec where

import Data.ByteString (ByteString)
import Data.FSEntries.Generators (genFSEntries)
import Data.FSEntries.IO (readFSEntriesFromFS, writeFSEntriesToFS)
import Data.FSEntries.Types
import Hedgehog
       (MonadGen, Property, (===), evalIO, forAll, property)
import Hedgehog.Gen (bytes)
import Hedgehog.Range (constant)
import System.IO.Temp (withSystemTempDirectory)

maxSize :: Int
maxSize = 4 * 1024

genFSEntries'
  :: MonadGen m
  => m (FSEntries () ByteString)
genFSEntries' = genFSEntries (pure ()) (bytes $ constant 0 maxSize)

hprop_io :: Property
hprop_io =
  property $ do
    entries <- forAll $ genFSEntries'
    roundtrippedEntries <- evalIO $ roundtripIO entries
    entries === roundtrippedEntries

roundtripIO :: FSEntries () ByteString -> IO (FSEntries () ByteString)
roundtripIO entries =
  withSystemTempDirectory "fs-entries-Data.FSEntries.IOSpec" $ \tmpdir -> do
    writeFSEntriesToFS tmpdir entries
    readFSEntriesFromFS tmpdir
