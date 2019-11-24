{-# LANGUAGE RankNTypes #-}

module Data.FSEntries.TypesSpec
  ( spec_typeclasses
  ) where

import Data.FSEntries.Generators
import Data.FSEntries.Types (FSEntries, mkDir, mkFSEntries, pruneEmptyDirs)
import Hedgehog (Gen)
import Hedgehog.Classes
  ( bifoldableLaws
  , bifunctorLaws
  , bitraversableLaws
  , foldableLaws
  , functorLaws
  , lawsCheckMany
  , traversableLaws
  )
import Hedgehog.Gen (integral)
import Hedgehog.Range (linear)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe)

spec_typeclasses :: Spec
spec_typeclasses = do
  describe "FSEntry" $
    it "obeys typeclass laws" $ do
      passed <-
        lawsCheckMany
          [ ( "FSEntry"
            , [ foldableLaws $ genFSEntry genInt
              , functorLaws $ genFSEntry genInt
              , traversableLaws $ genFSEntry genInt
              , bifoldableLaws genFSEntry
              , bifunctorLaws genFSEntry
              , bitraversableLaws genFSEntry
              ])
          ]
      passed `shouldBe` True
  describe "FSEntries" $
    it "obeys typeclass laws" $ do
      passed <-
        lawsCheckMany
          [ ( "FSEntries"
            , [ foldableLaws $ genFSEntries genInt
              , functorLaws $ genFSEntries genInt
              , traversableLaws $ genFSEntries genInt
              , bifoldableLaws genFSEntries
              , bifunctorLaws genFSEntries
              , bitraversableLaws genFSEntries
              ])
          ]
      passed `shouldBe` True
  describe "pruneEmptyDirs" $
    it "does" $ do
      let entries :: FSEntries () ()
          entries =
            mkFSEntries
              [ mkDir
                  "foo"
                  ()
                  [mkDir "bar" () [], mkDir "baz" () [mkDir "quux" () []]]
              ]
      pruneEmptyDirs entries `shouldBe` mkFSEntries []

genInt :: Gen Int
genInt = integral $ linear 0 1000
