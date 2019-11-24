module Data.FSEntries.ZipSpec
  ( hprop_interleave
  , hprop_override
  , spec_override
  ) where

import Data.ByteString (ByteString)
import Data.FSEntries.Generators (genFSEntries)
import Data.FSEntries.Types
import Data.FSEntries.Zip
import Data.Validation (Validation(..))
import Hedgehog (MonadGen, Property, (===), forAll, property)
import Hedgehog.Gen (bytes)
import Hedgehog.Range (constant)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe)

maxSize :: Int
maxSize = 4 * 1024

genFSEntries' :: MonadGen m => m (FSEntries () ByteString)
genFSEntries' = genFSEntries (pure ()) (bytes $ constant 0 maxSize)

hprop_interleave :: Property
hprop_interleave =
  property $ do
    entries <- forAll genFSEntries'
    Success entries === interleave [entries]
    Success entries === interleave [entries, emptyFSEntries]

hprop_override :: Property
hprop_override =
  property $ do
    entries <- forAll genFSEntries'
    Success entries === override [entries]
    Success entries === override [entries, emptyFSEntries]

spec_override :: Spec
spec_override =
  describe "Data.FSEntries.ZipSpec" $ do
      it "override works for top-level files" $ do
        let lhs = mkFSEntries [mkFile "foo" (1 :: Int)]
            rhs = mkFSEntries [mkFile "foo" 2]
        override [lhs, rhs] `shouldBe` Success rhs
      it "override works for secondary-level files" $ do
        let lhs = mkFSEntries [mkDir "bar" () [mkFile "foo" (1 :: Int)]]
            rhs = mkFSEntries [mkDir "bar" () [mkFile "foo" 2]]
        override [lhs, rhs] `shouldBe` Success rhs
      it "override fails on mismatch" $ do
        let lhs = mkFSEntries [mkDir "bar" () [mkFile "foo" (1 :: Int)]]
            rhs = mkFSEntries [mkFile "bar" 2]
        override [lhs, rhs] `shouldBe` Failure ()
