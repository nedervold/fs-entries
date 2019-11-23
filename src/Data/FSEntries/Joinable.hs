-- | An applicative functor where multiple applications can be reduced
-- to single applications.
{-# LANGUAGE DefaultSignatures #-}

module Data.FSEntries.Joinable where

import Control.Monad (join)
import Data.Validation

-- | An applicative functor where multiple applications can be reduced
-- to single applications.
class Applicative f =>
      Joinable f where
  join' :: f (f a) -> f a
  default join' :: (Monad f) =>
    f (f a) -> f a
  join' = join

instance Semigroup e =>
         Joinable (Validation e) where
  join' v =
    case v of
      Failure e -> Failure e
      Success v' -> v'
