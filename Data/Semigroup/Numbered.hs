-- |
-- Module      : Data.Semigroup.Numbered
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : LGPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}

module Data.Semigroup.Numbered ( SemigroupNo(..)
                               -- * The common directions
                               , SemigroupX, SemigroupY, SemigroupZ
                               -- * Infix ops (diagrams/hmatrix style)
                               , (|||), (===)
                               -- * Infix ops (ASCII-art style)
                               , (│), (──), (■), (┃), (━━), (██)
                               ) where

import GHC.TypeNats
import qualified Data.List.NonEmpty as NE
import Data.Foldable

import Data.Proxy

import Data.CallStack (HasCallStack)

class SemigroupNo (n :: Nat) g where
  sappendN :: proxy n -> g -> g -> g
  
  sconcatN :: proxy n -> NE.NonEmpty g -> g
  sconcatN = foldr1 . sappendN
  
  stimesN :: (Integral b, HasCallStack) => proxy n -> b -> g -> g
   -- Adapted from
   -- http://hackage.haskell.org/package/base-4.10.0.0/docs/src/Data.Semigroup.html#Semigroup
  stimesN p y₀ x₀
    | y₀ <= 0   = error "stimesN: positive multiplier expected"
    | otherwise = f x₀ y₀
   where
      f x y
        | even y = f (x <> x) (y `quot` 2)
        | y == 1 = x
        | otherwise = g (x <> x) (pred y  `quot` 2) x
      g x y z
        | even y = g (x <> x) (y `quot` 2) z
        | y == 1 = x <> z
        | otherwise = g (x <> x) (pred y `quot` 2) (x <> z)
      (<>) = sappendN p


type SemigroupX = SemigroupNo 0

infixr 6 │
-- | Horizontal concatenation. Fixity as of
--   <http://hackage.haskell.org/package/base-4.10.0.0/docs/Data-Semigroup.html#v:-60--62- the standard semigroup>.
--
--   @U+2502@ / Vim digraph @vv@.
(│) :: SemigroupX g => g -> g -> g
(│) = sappendN (Proxy :: Proxy 0)

infixr 3 ┃
-- | Horizontal concatenation. @U+2503@ / Vim digraph @VV@.
(┃) :: SemigroupX g => g -> g -> g
(┃) = sappendN (Proxy :: Proxy 0)

infixl 6 |||
-- | Horizontal concatenation. Fixity as
--   <http://hackage.haskell.org/package/diagrams-lib-1.4.1.2/docs/Diagrams-TwoD-Combinators.html#v:-61--61--61- in diagrams>.
(|||) :: SemigroupX g => g -> g -> g
(|||) = sappendN (Proxy :: Proxy 0)

type SemigroupY = SemigroupNo 1

infixr 5 ──
-- | Vertical concatenation. @U+2500@ / Vim digraph @hh@.
(──) :: SemigroupY g => g -> g -> g
(──) = sappendN (Proxy :: Proxy 1)

infixr 2 ━━
-- | Vertical concatenation. @U+2501@ / Vim digraph @HH@.
(━━) :: SemigroupY g => g -> g -> g
(━━) = sappendN (Proxy :: Proxy 1)

infixl 6 ===
-- | Vertical concatenation. Fixity as
--   <http://hackage.haskell.org/package/diagrams-lib-1.4.1.2/docs/Diagrams-TwoD-Combinators.html#v:-124--124--124- in diagrams>.
(===) :: SemigroupY g => g -> g -> g
(===) = sappendN (Proxy :: Proxy 1)

type SemigroupZ = SemigroupNo 2

infixr 4 ■
-- | z-concatenation. @U+25A0@ / Vim digraph @fS@.
(■) :: SemigroupZ g => g -> g -> g
(■) = sappendN (Proxy :: Proxy 2)

infixr 1 ██
-- | z-concatenation. @U+254B@ / Vim digraph @FB@.
(██) :: SemigroupZ g => g -> g -> g
(██) = sappendN (Proxy :: Proxy 2)

