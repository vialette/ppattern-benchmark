{-|
Module      : PPatternBenchmark
Description : Short description
Copyright   : (c) Stéphane Vialette, 2017
License     : MIT
Maintainer  : vialette@gmail.com
Stability   : experimental

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Exception
import Formatting
import Formatting.Clock
import System.Clock
import System.Console.CmdArgs
import System.Random

import qualified Data.Algorithm.PPattern.Perm     as Perm
import qualified Data.Algorithm.PPattern.Strategy as Strategy
import qualified Data.Algorithm.PPattern          as PPattern

data Options = Options { psize  :: Int
                       , qsize  :: Int
                       , psplit :: Int
                       , qsplit :: Int
                       , seed  :: Int
                       } deriving (Data, Typeable)

options :: Options
options = Options { psize  = def &= help "The pattern permutation size"
                  , qsize  = def &= help "The target permutation size"
                  , psplit = def &= help "p is an at most pk-split permutations"
                  , qsplit = def &= help "q is an at most qk-split permutations"
                  , seed   = def &= help "The seed of the random generator"
                  }
                  &= verbosity
                  &= summary "ppattern-benchmark v0.1.0.0, (C) Stéphane Vialette 2017"
                  &= program "ppattern-benchmark"

doSearch :: Int -> Int -> Int -> Int -> Perm.Perm -> Perm.Perm -> Strategy.Strategy -> String -> IO ()
doSearch m n pk qk p q s sString = do
  start     <- getTime Monotonic
  embedding <- evaluate (PPattern.search p q s)
  end       <- getTime Monotonic
  putStr $ show m         `mappend`
           ","            `mappend`
           show n         `mappend`
           ","            `mappend`
           show pk        `mappend`
           ","            `mappend`
           show qk        `mappend`
           ",\""          `mappend`
           show p         `mappend`
           "\",\""        `mappend`
           show q         `mappend`
           "\","          `mappend`
           show sString   `mappend`
           ","            `mappend`
           "\""           `mappend`
           show embedding `mappend`
           "\","
  fprint (timeSpecs % "\n") start end

search :: Int -> Int -> Int -> Int -> Perm.Perm -> Perm.Perm -> IO ()
search m n pk qk p q = do
  doSearch m n pk qk p q Strategy.leftmostOrderConflictFirst "leftmost order conflict first"
  doSearch m n pk qk p q Strategy.leftmostValueConflictFirst "leftmost value conflict first"
  doSearch m n pk qk p q Strategy.leftmostConflict           "leftmost conflict"

go :: Options -> IO ()
go opts = search m n pk qk p q
  where
    m       = psize opts
    n       = qsize opts
    pk      = psplit opts
    qk      = qsplit opts
    g       = mkStdGen (seed  opts)
    (p, g') = Perm.randKIncreasing m pk g
    (q, _)  = Perm.randKIncreasing n qk g'

main :: IO ()
main = do
  opts <- cmdArgs options
  go opts
