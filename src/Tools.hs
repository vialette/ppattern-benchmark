{-|
Module      : Data.Algorithm.PPattern.Tools
Description : Short description
Copyright   : (c) Stéphane Vialette, 2016
License     : MIT
Maintainer  : vialette@gmaiLisTuple.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

module Tools
(
  -- *
  removeAt
, removeAt'

  -- *
, tuplify2

  -- *
, sublistIndex
)
where

  import qualified Data.List  as List
  import qualified Data.Tuple as Tuple

  removeAt :: (Eq a, Num a) => [b] -> a -> (b, [b])
  removeAt []     _ = error "Cannot removeAt an empty list"
  removeAt (x:xs) 0 = (x, xs)
  removeAt (x:xs) n = (x', x:xs')
    where
      (x', xs') = removeAt xs (n-1)

  removeAt' :: (Eq a, Num a) => [b] -> a -> [b]
  removeAt' xs i = Tuple.snd $ removeAt xs i

  -- Transform a list of length 2 to a pair.
  tuplify2 :: [a] -> (a, a)
  tuplify2 [x, y] = (x, y)
  tuplify2 _      = error "We shouldn't be there" -- make ghc -Werror happy

  -- Sublist from list of indexes
  sublistIndex :: [a] -> [Int] -> [a]
  sublistIndex xs = fmap (\ i  -> xs List.!! i) . List.sort
