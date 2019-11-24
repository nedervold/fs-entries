module Data.FSEntries.FunctorSpec
  ( hprop_expandRoundtrip
  ) where

import Data.ByteString (ByteString)
import Data.FSEntries.Generators (genFSEntries)
import Data.FSEntries.Types (FSEntries)
import Data.FSEntries.Functor
import Data.Validation (Validation(..))
import Hedgehog (MonadGen, Property, (===), forAll, property)
import Hedgehog.Gen (bytes)
import Hedgehog.Range (constant)

maxSize :: Int
maxSize = 4 * 1024

genFSEntries' :: MonadGen m => m (FSEntries () ByteString)
genFSEntries' = genFSEntries (pure ()) (bytes $ constant 0 maxSize)

hprop_expandRoundtrip :: Property
hprop_expandRoundtrip =
  property $ do
    entries <- forAll genFSEntries'
    let entriesF = expandEntries entries
    let entries' :: Validation () (FSEntries () ByteString)
        entries' = contractEntries entriesF
    Success entries === entries'
