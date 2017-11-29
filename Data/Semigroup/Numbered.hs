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
{-# LANGUAGE FlexibleInstances         #-}

module Data.Semigroup.Numbered ( SemigroupNo(..)
                               -- * The common directions
                               , SemigroupX, SemigroupY, SemigroupZ
                               -- * Infix ops (diagrams/hmatrix style)
                               , (|||), (===)
                               -- * Infix ops (ASCII-art style)
                               , (│), (──), (■), (┃), (━━), (██)
                               ) where

import GHC.TypeLits
import qualified Data.List.NonEmpty as NE
import qualified Data.Foldable as Foldable

import Data.Proxy
import Data.Void

import Data.CallStack (HasCallStack)

class SemigroupNo (n :: Nat) g where
  sappendN :: proxy n -> g -> g -> g
  sappendN p x y = sconcatN p $ x NE.:|[y]
  
  sconcatN :: proxy n -> NE.NonEmpty g -> g
  sconcatN = Foldable.foldr1 . sappendN
  
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

instance (SemigroupNo n g) => SemigroupNo n (a -> g) where
  sappendN p f g x = sappendN p (f x) (g x)
  sconcatN p fs x = sconcatN p $ ($x)<$>fs
  stimesN p n f = stimesN p n . f

instance (SemigroupNo n g) => SemigroupNo n (Maybe g) where
  sappendN _ Nothing b = b
  sappendN _ a Nothing = a
  sappendN p (Just a) (Just b) = Just $ sappendN p a b
  stimesN _ _ Nothing = Nothing
  stimesN p n (Just a) = Just $ stimesN p n a

instance SemigroupNo n () where
  sappendN _ () () = ()
  sconcatN _ _ = ()
  stimesN _ _ () = ()

instance SemigroupNo n (Proxy x) where
  sappendN _ Proxy Proxy = Proxy
  sconcatN _ _ = Proxy
  stimesN _ _ Proxy = Proxy

instance SemigroupNo n Void where
  sappendN _ = absurd
  stimesN _ _ = absurd

instance SemigroupNo 0 [Void] where sappendN _ [] [] = []
instance SemigroupNo 0 [()] where sappendN _ = (++)
instance SemigroupNo 0 [Char] where sappendN _ = (++)
instance SemigroupNo 0 [Int] where sappendN _ = (++)
instance SemigroupNo 0 [Integer] where sappendN _ = (++)
instance SemigroupNo 0 [Float] where sappendN _ = (++)
instance SemigroupNo 0 [Double] where sappendN _ = (++)
instance SemigroupNo 0 [Rational] where sappendN _ = (++)
instance SemigroupNo 0 [Maybe a] where sappendN _ = (++)
instance (SemigroupNo 0 [a]) => SemigroupNo 0 [[a]] where
  sappendN _ [] ys = ys
  sappendN _ xs [] = xs
  sappendN p (x:xs) (y:ys) = sappendN p x y : sappendN p xs ys

instance SemigroupNo 1 [[Void]] where sappendN _ = (++)
instance SemigroupNo 1 [[()]] where sconcatN _ = paddedLines () . concat
instance SemigroupNo 1 [[Char]] where sconcatN _ = paddedLines ' ' . concat
instance SemigroupNo 1 [[Int]] where sconcatN _ = paddedLines 0 . concat
instance SemigroupNo 1 [[Integer]] where sconcatN _ = paddedLines 0 . concat
instance SemigroupNo 1 [[Float]] where sconcatN _ = paddedLines 0 . concat
instance SemigroupNo 1 [[Double]] where sconcatN _ = paddedLines 0 . concat
instance SemigroupNo 1 [[Rational]] where sconcatN _ = paddedLines 0 . concat
instance SemigroupNo 1 [[Maybe a]] where sconcatN _ = paddedLines Nothing . concat
instance (SemigroupNo 1 [[a]]) => SemigroupNo 1 [[[a]]] where
  sappendN _ [] ys = ys
  sappendN _ xs [] = xs
  sappendN p (x:xs) (y:ys) = sappendN p x y : sappendN p xs ys

paddedLines :: a -> [[a]] -> [[a]]
paddedLines padr xs = mkPadded <$> xs
 where mkPadded cs = cs ++ replicate (paddingLen - length cs) padr
       paddingLen = maximum $ length <$> xs

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

