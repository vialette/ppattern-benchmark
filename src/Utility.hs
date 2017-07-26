module Utility
where

  import qualified Data.List     as List
  import qualified Data.Foldable as Foldable
  import qualified System.Random

  import qualified Data.Algorithm.PPattern.Perm as Perm

  import qualified Random
  import qualified IntPartition


  -- 'mkIncreasings xs' constructs increasing lists, where the length of each
  -- list is given by the elements of 'xs'.
  mkIncreasings :: System.Random.RandomGen g => [Int] -> g -> ([[Int]], g)
  mkIncreasings ls = mkIncreasingsAux [] [1..n] ls
    where
      n = Foldable.sum ls

  mkIncreasingsAux :: System.Random.RandomGen g =>
    [[Int]] -> [Int] -> [Int] -> g -> ([[Int]], g)
  mkIncreasingsAux acc _  []       g = (acc, g)
  mkIncreasingsAux acc xs (l : ls) g = mkIncreasingsAux (ys : acc) (xs List.\\ ys) ls g'
    where
      (ys, g') = Random.sample l xs g

  -- {-|
  --   'randKIncreasing' takes two integers 'n' and 'k' and a generator 'g'.
  --   It returns a random permutation of length 'n' that is the union of 'k'
  --   increasings sequences, together with a new generatoRandom.
  -- -}
  randKIncreasing :: System.Random.RandomGen g => Int -> Int -> g -> (Perm.Perm, g)
  randKIncreasing n k g
    | k > n     = (Perm.empty, g)
    | otherwise = (p, g''')
    where
      -- rand int partition
      (intPartition, g') = IntPartition.randIntPartition n k g
      partitionAsList = IntPartition.toList intPartition
      (partitionAsIncreasingLists, g'') = mkIncreasings partitionAsList g'

      -- random shuffle
      (xs, g''') = Random.randShuffle partitionAsIncreasingLists g''

      -- make permutation
      p = Perm.mk xs
