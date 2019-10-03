{-# LANGUAGE RankNTypes #-}

module Data.FSEntries.TypesSpec where

import Data.FSEntries.Generators
import Hedgehog (Gen)
import Hedgehog.Classes
       (bifoldableLaws, bifunctorLaws, bitraversableLaws, foldableLaws,
        functorLaws, lawsCheckMany, traversableLaws)
import Hedgehog.Gen (integral)
import Hedgehog.Range (linear)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe)

spec_typeclasses :: Spec
spec_typeclasses = do
  describe "Data.FSEntries.Types" $ do
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
          , ( "FSEntries"
            , [ foldableLaws $ genFSEntries genInt
              , functorLaws $ genFSEntries genInt
              , traversableLaws $ genFSEntries genInt
              , bifoldableLaws genFSEntries
              , bifunctorLaws genFSEntries
              , bitraversableLaws genFSEntries
              ])
          ]
      passed `shouldBe` True

genInt :: Gen Int
genInt = integral $ linear 0 1000
