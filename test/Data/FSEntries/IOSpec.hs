{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.FSEntries.IOSpec
  ( hprop_roundtripIO
  , hprop_roundtripLazyIO
  , spec_writeFileIfChanged
  ) where

import Control.Concurrent (threadDelay)
import Data.ByteString (ByteString, hPut)
import Data.FSEntries.Generators (genFSEntries)
import Data.FSEntries.IO
  ( lazyReadFSEntriesFromFS
  , readFSEntriesFromFS
  , writeFSEntriesToFS
  , writeFileIfChanged
  )
import Data.FSEntries.Types
import Hedgehog (MonadGen, Property, (===), evalIO, forAll, property)
import Hedgehog.Gen (bytes)
import Hedgehog.Range (constant)
import System.Directory (getModificationTime)
import System.IO (hClose)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

maxSize :: Int
maxSize = 4 * 1024

genFSEntries' :: MonadGen m => m (FSEntries () ByteString)
genFSEntries' = genFSEntries (pure ()) (bytes $ constant 0 maxSize)

hprop_roundtripIO :: Property
hprop_roundtripIO =
  property $ do
    entries <- forAll genFSEntries'
    roundtrippedEntries <- evalIO $ roundtripIO entries
    entries === roundtrippedEntries

roundtripIO :: FSEntries () ByteString -> IO (FSEntries () ByteString)
roundtripIO entries =
  withSystemTempDirectory "fs-entries-Data.FSEntries.IOSpec" $ \tmpdir -> do
    writeFSEntriesToFS tmpdir entries
    readFSEntriesFromFS tmpdir

hprop_roundtripLazyIO :: Property
hprop_roundtripLazyIO =
  property $ do
    entries <- forAll genFSEntries'
    roundtrippedEntries <- evalIO $ roundtripLazyIO entries
    entries === roundtrippedEntries

roundtripLazyIO :: FSEntries () ByteString -> IO (FSEntries () ByteString)
roundtripLazyIO entries =
  withSystemTempDirectory "fs-entries-Data.FSEntries.IOSpec" $ \tmpdir -> do
    writeFSEntriesToFS tmpdir entries
    entries' :: FSEntries () (IO ByteString) <- lazyReadFSEntriesFromFS tmpdir
    sequence entries'

spec_writeFileIfChanged :: Spec
spec_writeFileIfChanged =
  describe "writeFileIfChanged" $ do
    it "doesn't write the same data" $
      withSystemTempFile
        "fs-entries.Data.FSEntries.IOSpec.spec_writeFileIfChanged" $ \fp h -> do
        hPut h "old"
        hClose h
        oldModTime <- getModificationTime fp
        threadDelay twoSecs
        writeFileIfChanged fp "old"
        newBS <- readFile fp
        newModTime <- getModificationTime fp
        (newModTime, newBS) `shouldBe` (oldModTime, "old")
    it "does write different data" $
      withSystemTempFile
        "fs-entries.Data.FSEntries.IOSpec.spec_writeFileIfChanged" $ \fp h -> do
        hPut h "old"
        hClose h
        oldModTime <- getModificationTime fp
        threadDelay twoSecs
        writeFileIfChanged fp "new"
        newBS <- readFile fp
        newModTime <- getModificationTime fp
        newModTime `shouldNotBe` oldModTime
        newBS `shouldBe` "new"
  where
    twoSecs = 2 * 1000 * 1000
