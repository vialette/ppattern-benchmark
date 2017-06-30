{-|
Module      : PPatternBenchmark
Description : Short description
Copyright   : (c) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-1017
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

import qualified Data.Algorithm.PPattern.APerm    as APerm
import qualified Data.Algorithm.PPattern          as PPattern
import qualified Utility

data Options = Options { psize  :: Int
                       , qsize  :: Int
                       , psplit :: Int
                       , qsplit :: Int
                       , seed  :: Int
                       } deriving (Data, Typeable)

options :: Options
options = Options { psize  = def &= help "The pattern permutation size"
                  , qsize  = def &= help "The target permutation size"
                  , psplit = def &= help "p is the union of at most psplit increasingss"
                  , qsplit = def &= help "q is the union of at most qsplit increasings"
                  , seed   = def &= help "The seed of the random generator"
                  }
                  &= verbosity
                  &= summary "ppattern-benchmark v0.1.0.0, (C) Laurent Bulteau, Romeo Rizzi, Stéphane Vialette, 2016-1017"
                  &= program "ppattern-benchmark"

doSearch :: (Ord a, Show a) => Int -> Int -> Int -> Int -> APerm.APerm a -> APerm.APerm a -> IO ()
doSearch m n pk qk p q = do
  start     <- getTime Monotonic
  embedding <- evaluate (PPattern.search p q)
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
           "\""           `mappend`
           show embedding `mappend`
           "\","
  fprint (timeSpecs % "\n") start end

search :: (Ord a, Show a) => Int -> Int -> Int -> Int -> APerm.APerm a -> APerm.APerm a -> IO ()
search m n pk qk p q = do
  -- doSearch m n pk qk p q Strategy.leftmostOrderConflictFirst "leftmost order conflict first"
  -- doSearch m n pk qk p q Strategy.leftmostValueConflictFirst "leftmost value conflict first"
  doSearch m n pk qk p q

go :: Options -> IO ()
go opts = search m n pk qk p q
  where
    m       = psize opts
    n       = qsize opts
    pk      = psplit opts
    qk      = qsplit opts
    g       = mkStdGen (seed  opts)
    (p, g') = Utility.randKIncreasing m pk g
    (q, _)  = Utility.randKIncreasing n qk g'

main :: IO ()
main = do
  opts <- cmdArgs options
  go opts
