module Data.FSEntries.ZipSpec
  ( hprop_interleaveIdentity
  , hprop_interleaveEqIdentity
  , hprop_overrideIdentity
  , spec_interleave
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
import Test.Hspec (Spec, describe, it, shouldBe)

maxSize :: Int
maxSize = 4 * 1024

genFSEntries' :: MonadGen m => m (FSEntries () ByteString)
genFSEntries' = genFSEntries (pure ()) (bytes $ constant 0 maxSize)

hprop_interleaveIdentity :: Property
hprop_interleaveIdentity =
  property $ do
    entries <- forAll genFSEntries'
    Right entries === interleave' [entries]
    Right entries === interleave' [entries, emptyFSEntries]

hprop_interleaveEqIdentity :: Property
hprop_interleaveEqIdentity =
  property $ do
    entries <- forAll genFSEntries'
    Right entries === interleaveEq' [entries]
    Right entries === interleaveEq' [entries, entries]
    Right entries === interleaveEq' [entries, emptyFSEntries]

hprop_overrideIdentity :: Property
hprop_overrideIdentity =
  property $ do
    entries <- forAll genFSEntries'
    Right entries === override' [entries]
    Right entries === override' [entries, emptyFSEntries]
    Right entries === override' [entries, emptyFSEntries, entries]

-- hprop_interleave :: Property
-- hprop_interleave =
--   property $ do
--     entries <- forAll genFSEntries'
--     Success entries === interleave [entries]
--     Success entries === interleave [entries, emptyFSEntries]
--
-- hprop_override :: Property
-- hprop_override =
--   property $ do
--     entries <- forAll genFSEntries'
--     Success entries === override [entries]
--     Success entries === override [entries, emptyFSEntries]
--     Success entries === override [entries, emptyFSEntries, entries]
spec_interleave :: Spec
spec_interleave =
  describe "interleave" $
  it "works for empty dirs" $ do
    let lhs = mkFSEntries [mkDir "bar" () []]
        rhs = mkFSEntries [mkDir "bar" () []]
    let res :: Validation () (FSEntries () ())
        res = interleave [lhs, rhs]
    res `shouldBe` Success lhs

spec_override :: Spec
spec_override =
  describe "override" $ do
    it "works for top-level files" $ do
      let lhs = mkFSEntries [mkFile "foo" (1 :: Int)]
          rhs = mkFSEntries [mkFile "foo" 2]
      override [lhs, rhs] `shouldBe` Success rhs
    it "works for secondary-level files" $ do
      let lhs = mkFSEntries [mkDir "bar" () [mkFile "foo" (1 :: Int)]]
          rhs = mkFSEntries [mkDir "bar" () [mkFile "foo" 2]]
      override [lhs, rhs] `shouldBe` Success rhs
    it "fails on mismatch" $ do
      let lhs = mkFSEntries [mkDir "bar" () [mkFile "foo" (1 :: Int)]]
          rhs = mkFSEntries [mkFile "bar" 2]
      override [lhs, rhs] `shouldBe` Failure ()
