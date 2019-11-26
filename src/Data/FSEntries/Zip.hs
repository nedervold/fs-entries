-- | Functions for zipping two 'FSEntries' together.
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.FSEntries.Zip where {-  ( -- * merges
    interleave
  , override
    -- * building blocks
  , MergeFunc
  , mergeAllFSEntries
  ) -}

import Control.Applicative (liftA2)
import Control.Monad.Reader (Reader, ask, local, runReader)
import Data.FSEntries.Functor
import Data.FSEntries.Joinable
import Data.FSEntries.Types
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Validation (Validation(..), toEither)
import System.FilePath ((</>))
import Text.Printf (printf)

-- TODO Perhaps MergeFunc could be better implemented with a datatype
-- to remove the impossible state: there can be exactly one or two
-- FSEntryFs. Do we need the one-FSEntryF case?  In all the versions I
-- can think of, we just use it if it's a singleton.  Ponder.
-- | Merge optional 'FSEntry's.  They are optional because when
-- merging two FSEntries, there may or may not be an 'FSEntry' at any
-- given name.  The result is an 'Applicative' value to allow for
-- errors.  Will never be called with two 'Nothing' values.
type MergeFunc f' d f
   = Maybe (FSEntryF f' d f) -> Maybe (FSEntryF f' d f) -> f' (FSEntryF f' d f)

type MergeFunc' d f
   = Maybe (FSEntryF P d f) -> Maybe (FSEntryF P d f) -> P (FSEntryF P d f)

-- original
type V = Validation ()

-- potential message
type P = Validation [FilePath -> String]

-- FilePath-labeled message
type L = Validation [String]

interleaveMergeFunc :: MergeFunc V () f
interleaveMergeFunc Nothing Nothing = error "interleaveMergeFunc: impossible"
interleaveMergeFunc Nothing (Just rhs) = pure rhs
interleaveMergeFunc (Just lhs) Nothing = pure lhs
interleaveMergeFunc (Just (DirF () entries)) (Just (DirF () entries')) =
  pure $ DirF () $ mergeFSEntriesF interleaveMergeFunc entries entries'
interleaveMergeFunc _ _ = Failure ()

overrideMergeFunc :: forall f. MergeFunc V () f
overrideMergeFunc Nothing Nothing = error "overrideMergeFunc: impossible"
overrideMergeFunc Nothing (Just rhs) = pure rhs
overrideMergeFunc (Just lhs) Nothing = pure lhs
overrideMergeFunc (Just (FileF _)) (Just f@(FileF _)) = pure f
overrideMergeFunc (Just (DirF () entries)) (Just (DirF () entries')) =
  pure $ DirF () $ mergeFSEntriesF overrideMergeFunc entries entries'
overrideMergeFunc _ _ = Failure ()

------------------------------------------------------------
interleaveMergeFunc' :: MergeFunc' () f
interleaveMergeFunc' Nothing Nothing = error "interleaveMergeFunc': impossible"
interleaveMergeFunc' Nothing (Just rhs) = pure rhs
interleaveMergeFunc' (Just lhs) Nothing = pure lhs
interleaveMergeFunc' (Just (DirF () entries)) (Just (DirF () entries')) =
  pure $ DirF () $ mergeFSEntriesF interleaveMergeFunc' entries entries'
-- what if they're both files with equal data?
interleaveMergeFunc' _ _ = Failure [printf "interleave: conflict at %s"]

interleaveEqMergeFunc' :: Eq f => MergeFunc' () f
interleaveEqMergeFunc' Nothing Nothing =
  error "interleaveEqMergeFunc': impossible"
interleaveEqMergeFunc' Nothing (Just rhs) = pure rhs
interleaveEqMergeFunc' (Just lhs) Nothing = pure lhs
interleaveEqMergeFunc' (Just (DirF () entries)) (Just (DirF () entries')) =
  pure $ DirF () $ mergeFSEntriesF interleaveEqMergeFunc' entries entries'
interleaveEqMergeFunc' (Just (FileF f)) (Just file@(FileF f')) =
  if f == f'
    then pure file
    else Failure [printf "interleaveEq: conflict at %s"]
interleaveEqMergeFunc' _ _ = Failure [printf "interleaveEq: conflict at %s"]

overrideMergeFunc' :: forall f. MergeFunc' () f
overrideMergeFunc' Nothing Nothing = error "overrideMergeFunc': impossible"
overrideMergeFunc' Nothing (Just rhs) = pure rhs
overrideMergeFunc' (Just lhs) Nothing = pure lhs
overrideMergeFunc' (Just (FileF _)) (Just f@(FileF _)) = pure f
overrideMergeFunc' (Just (DirF () entries)) (Just (DirF () entries')) =
  pure $ DirF () $ mergeFSEntriesF overrideMergeFunc' entries entries'
overrideMergeFunc' (Just (FileF _)) (Just (DirF _ _)) =
  Failure [printf "override: file-directory conflict at %s"]
overrideMergeFunc' (Just (DirF _ _)) (Just (FileF _)) =
  Failure [printf "override: directory-file conflict at %s"]

------------------------------------------------------------
mergeFSEntriesF ::
     forall f' d f. Joinable f'
  => MergeFunc f' d f
  -> FSEntriesF f' d f
  -> FSEntriesF f' d f
  -> FSEntriesF f' d f
mergeFSEntriesF mergeEntry (FSEntriesF m) (FSEntriesF m') =
  FSEntriesF $ M.fromList [(key, mergeEntryFor key) | key <- S.toList keys]
  where
    mergeEntryFor :: String -> f' (FSEntryF f' d f)
    mergeEntryFor key = join' $ liftA2 mergeEntry mV mV'
      where
        mV, mV' :: f' (Maybe (FSEntryF f' d f))
        mV = sequenceA $ M.lookup key m
        mV' = sequenceA $ M.lookup key m'
    keys :: S.Set String
    keys = M.keysSet m `S.union` M.keysSet m'

mergeAllFSEntries ::
     forall f' d f. Joinable f'
  => MergeFunc f' d f
  -> [FSEntries d f]
  -> f' (FSEntries d f)
mergeAllFSEntries mergeEntriesF entriesList =
  contractEntries $ foldl mergeMixedEntries emptyFSEntriesF entriesList
  where
    mergeMixedEntries :: FSEntriesF f' d f -> FSEntries d f -> FSEntriesF f' d f
    mergeMixedEntries entriesF entries =
      mergeFSEntriesF mergeEntriesF entriesF (expandEntries entries)

------------------------------------------------------------
mergeAllFSEntries' ::
     forall d f. MergeFunc' d f -> [FSEntries d f] -> L (FSEntries d f)
mergeAllFSEntries' mergeEntriesF entriesList =
  contractEntries $
  applyLabels $ foldl mergeMixedEntries emptyFSEntriesF entriesList
  where
    mergeMixedEntries :: FSEntriesF P d f -> FSEntries d f -> FSEntriesF P d f
    mergeMixedEntries entriesF entries =
      mergeFSEntriesF mergeEntriesF entriesF (expandEntries entries)

type R = Reader FilePath

applyLabels :: FSEntriesF P d f -> FSEntriesF L d f
applyLabels entries = runReader (applyLabelsEntries entries) ""

-- I think it's best just to plow through with the implementation.
-- It seems to be sui generis.
applyLabelsEntries :: forall d f. FSEntriesF P d f -> R (FSEntriesF L d f)
applyLabelsEntries (FSEntriesF m) = do
  pairs' <- mapM f pairs
  return $ FSEntriesF $ M.fromList pairs'
  where
    pairs :: [(String, P (FSEntryF P d f))]
    pairs = M.toList m
    f :: (String, P (FSEntryF P d f)) -> R (String, L (FSEntryF L d f))
    f (nm, p) =
      local (</> nm) $ do
        l <- g p
        return (nm, l)
    g :: P (FSEntryF P d f) -> R (L (FSEntryF L d f))
    g p =
      case p of
        Success entry -> Success <$> applyLabelsEntry entry
        Failure msgs -> do
          fp <- ask
          return $ Failure $ map ($fp) msgs

applyLabelsEntry :: FSEntryF P d f -> R (FSEntryF L d f)
applyLabelsEntry (FileF f) = return $ FileF f
applyLabelsEntry (DirF d entries) = DirF d <$> applyLabelsEntries entries

------------------------------------------------------------
interleave :: [FSEntries () f] -> V (FSEntries () f)
interleave = mergeAllFSEntries interleaveMergeFunc

override :: [FSEntries () f] -> V (FSEntries () f)
override = mergeAllFSEntries overrideMergeFunc

------------------------------------------------------------

-- | Interleave 'FSEntries' values, returning either a list of
-- conflicts or the merged result.  If two values both have files at
-- the same path, that is a file-file conflict.  If one has a file and
-- the other a directory, that is a directory-file or file-directory
-- conflict.  This is intended for the case where you have a
-- pre-determined structure and you are combining the pieces that make
-- it up.  Any conflict indicates an error.
interleave' :: [FSEntries () f] -> Either [String] (FSEntries () f)
interleave' = toEither . mergeAllFSEntries' interleaveMergeFunc'

-- | Interleave 'FSEntries' values returning the merged result or
-- throwing an error on conflict.  The tag is added to the error
-- message.
sureInterleave' :: String -> [FSEntries () f] -> FSEntries () f
sureInterleave' tag = either mkErr id . interleave'
    where
    mkErr msgs = error $ unlines (tag : msgs)

-- | Interleave 'FSEntries' values, returning either a list of
-- conflicts or the merged result.  If two values both have files at
-- the same path __whose data is not the same__, that is a file-file
-- conflict.  If one has a file and the other a directory, that is a
-- directory-file or file-directory conflict.  This is intended for
-- the case where you have a pre-determined structure and you are
-- combining the pieces that make it up.  Any conflict indicates an
-- error.
interleaveEq' :: Eq f => [FSEntries () f] -> Either [String] (FSEntries () f)
interleaveEq' = toEither . mergeAllFSEntries' interleaveEqMergeFunc'

-- | Interleave 'FSEntries' values returning the merged result or
-- throwing an error on conflict.  The tag is added to the error
-- message.
sureInterleaveEq' :: Eq f => String -> [FSEntries () f] -> FSEntries () f
sureInterleaveEq' tag = either mkErr id . interleaveEq'
    where
    mkErr msgs = error $ unlines (tag : msgs)

-- | Combine 'FSEntries' by interleaving them, but allowing later
-- file entries to override earlier ones.  If two values at the same
-- path both have files at the same path, the rightmost one is taken.
-- If one has a file and the other a directory, that is a
-- directory-file or file-directory conflict.  This is intended for
-- the case where you have a pre-determined structure and you are
-- combining the pieces that make it up, but allowing for
-- customization of file contents.
override' :: [FSEntries () f] -> Either [String] (FSEntries () f)
override' = toEither . mergeAllFSEntries' overrideMergeFunc'

-- | Combine 'FSEntries' values returning the merged result or
-- throwing an error on conflict.  The tag is added to the error
-- message.
sureOverrideEq' :: Eq f => String -> [FSEntries () f] -> FSEntries () f
sureOverrideEq' tag = either mkErr id . override'
    where
    mkErr msgs = error $ unlines (tag : msgs)
