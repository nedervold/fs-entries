{-# LANGUAGE RankNTypes #-}

module Data.FSEntries.TypesSpec
  ( hprop_lookup, spec_typeclasses
  ) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.FSEntries.Generators (genFSEntries, genFSEntry)
import Data.FSEntries.Types
  ( FSEntries
  , FSEntry(..)
  , (<//>)
  , lookup  , mkDir
  , mkFSEntries
  , mkFile
  , pruneEmptyDirs
  , toEntryList
  , tupleWithPath
  )
import qualified Data.Set as S
import Hedgehog (Gen, MonadGen, Property, (===), forAll, property)
import Hedgehog.Classes
  ( bifoldableLaws
  , bifunctorLaws
  , bitraversableLaws
  , foldableLaws
  , functorLaws
  , lawsCheckMany
  , traversableLaws
  )
import Hedgehog.Gen (bytes, integral)
import Hedgehog.Range (constant, linear)
import Prelude                   hiding(lookup)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe)

maxSize :: Int
maxSize = 4 * 1024

genFSEntries' :: MonadGen m => m (FSEntries () ByteString)
genFSEntries' = genFSEntries (pure ()) (bytes $ constant 0 maxSize)

hprop_lookup :: Property
hprop_lookup =
  property $ do
    entries <- forAll genFSEntries'
    let entryList = toEntryList entries
    forM_ entryList $ \(path, eData) ->
      (dataOf <$> lookup path entries) === Just eData
  where
    dataOf (Dir d _) = Left d
    dataOf (File f) = Right f

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
  describe "nest" $ do
    it "nests the entries into the directory" $
      "foo/bar/quux" <//> baseEntries `shouldBe` expected
    it "ignores trailing slashes" $
      "foo/bar/quux/" <//> baseEntries `shouldBe` expected
    it "ignores leading slashes" $
      "/foo/bar/quux/" <//> baseEntries `shouldBe` expected
    it "does nothing on empty string" $
      "" <//> baseEntries `shouldBe` baseEntries
    it "does nothing on a single slash" $
      "/" <//> baseEntries `shouldBe` baseEntries
  describe "tupleWithPath" $
    it "passes sanity check" $
    tupleWithPath expected `shouldBe`
    mkFSEntries
      [ mkDir
          "foo"
          ("foo", ())
          [ mkDir
              "bar"
              ("foo/bar", ())
              [ mkDir
                  "quux"
                  ("foo/bar/quux", ())
                  [mkFile "test.txt" ("foo/bar/quux/test.txt", ())]
              ]
          ]
      ]
  describe "tupleWithPath" $
    it "passes sanity check" $
    S.fromList (toEntryList expected) `shouldBe`
    S.fromList
      [ ("foo", Left ())
      , ("foo/bar", Left ())
      , ("foo/bar/quux", Left ())
      , ("foo/bar/quux/test.txt", Right ())
      ]
  where
    baseEntries :: FSEntries () ()
    baseEntries = mkFSEntries [mkFile "test.txt" ()]
    expected :: FSEntries () ()
    expected =
      mkFSEntries
        [ mkDir
            "foo"
            ()
            [mkDir "bar" () [mkDir "quux" () [mkFile "test.txt" ()]]]
        ]

genInt :: Gen Int
genInt = integral $ linear 0 1000
