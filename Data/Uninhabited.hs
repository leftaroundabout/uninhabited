-- |
-- Module      : Data.Uninhabited
-- Copyright   : (c) Justus Sagemüller 2020
-- License     : Apache
-- 
-- Maintainer  : (@) jsag $ hvl.no
-- Stability   : experimental
-- Portability : portable
-- 



module Data.Uninhabited where

import Data.Void
import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty(..))

class Uninhabited o where
  explosion :: o -> Void

-- | Generalised version of 'absurd'.
quodlibet :: Uninhabited o => o -> a
quodlibet = absurd . explosion

instance Uninhabited Void where
  explosion = id
instance Uninhabited o => Uninhabited (Identity o) where
  explosion (Identity h) = explosion h
instance Uninhabited o => Uninhabited (NonEmpty o) where
  explosion (h:|_) = explosion h
