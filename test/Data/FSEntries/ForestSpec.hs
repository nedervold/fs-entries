module Data.FSEntries.ForestSpec
  ( hprop_forestRoundtripping
  ) where

import Data.FSEntries.Forest
import Data.FSEntries.Generators
import Data.FSEntries.Types
import Data.Functor.Identity (Identity(..))
import Hedgehog (MonadTest, Property, forAll, property, tripping)
import Hedgehog.Gen (int, lower)
import Hedgehog.Range (constant)

-- | Test that conversion of an 'FSEntries' to a rose forest and back
-- does not change it.
hprop_forestRoundtripping :: Property
hprop_forestRoundtripping =
  property $ do
    entries <- forAll $ genFSEntries lower $ int $ constant 1 1000
    trippingToForestAndBack entries

trippingToForestAndBack ::
     (MonadTest m, Eq d, Eq f, Show d, Show f) => FSEntries d f -> m ()
trippingToForestAndBack entries =
  tripping entries entriesToForest (Identity . forestToEntries)
