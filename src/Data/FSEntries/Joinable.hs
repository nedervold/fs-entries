-- | An applicative functor where multiple applications can be reduced
-- to single applications.
{-# LANGUAGE DefaultSignatures #-}

module Data.FSEntries.Joinable
  ( Joinable(..)
  ) where

import Control.Monad (join)
import Data.Validation (Validation(..), valueOr)

-- | An applicative functor where multiple applications can be reduced
-- to single applications.  All monads are 'Joinable' via 'join'; not
-- all 'Applicative' are.
class Applicative f =>
      Joinable f
  where
  join' :: f (f a) -> f a
  default join' :: (Monad f) =>
    f (f a) -> f a
  join' = join

instance Semigroup e => Joinable (Validation e) where
  join' = valueOr Failure
