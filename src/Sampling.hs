{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Sampling(
  sampleFromBins,
  sampleUniformally
  ) where

import Data.List (transpose,nub,zipWith5,elemIndices)
import Data.Map.Strict (fromList)
import System.Random
import System.Random.Shuffle(shuffle')
import System.Random.TF (TFGen, seedTFGen)
import Numeric.Interval (Interval,inf,sup,interval)
import Data.Maybe (fromMaybe)
import Control.Monad.State.Lazy (runState,evalState)

import L1
import Annotizer


randomList :: (Random a) => (a,a) -> Int -> TFGen -> [a]
randomList bnds n = take n . randomRs bnds

-- I am not happy with this sampling methodology for small programs
sampleUniformally:: L1 -> Int -> [L1]
sampleUniformally e ns = map (pick $ localLattice e) $ nub rns
  where
    rns = transpose $ zipWith5 (\n a b c d -> randomList (0,n-1) ns $ seedTFGen (a,b,c,d)) tns [0..] [11..] [22..] [33..]
    tns = map typeNodesCount $ evalState (genTypeInfo undefined e) 0

genIntervals :: Double -> Double -> [Interval Int]
genIntervals intervalsCount intervalWidth =
  if intervalWidth > 1
  then genIntervals1 intervalsCount intervalWidth
  else genIntervals2 intervalsCount intervalWidth
  where g = fromMaybe $ error "genIntervals: internal error"
        genIntervals1 ic iw =
          if ic > 1
          then g (interval (ceiling $ (ic-1)*iw) (floor $ ic*iw)):genIntervals1 (ic-1) iw
          else [g $ interval 0 (floor iw)]
        genIntervals2 ic iw =
          if ic > 1
          then g (interval (ceiling $ (ic-1)*iw) (ceiling $ ic*iw)):genIntervals2 (ic-1) iw
          else [g $ interval 0 (ceiling iw)]

-- | Sample partially-typed versions from equally-sized bins of type preciseness
sampleFromBins :: L1     -- ^ The fully-statically typed AST to sample from
       -> Int    -- ^ The number of samples in each bin 
       -> Double -- ^ The number of bins
       -> [[L1]] -- ^ The list of bins, where each is a list of samples
sampleFromBins ast ns nb =
  let (typeInfo,typesNodesCount) = runState (genTypeInfo undefined ast) 0
  in sampleMN ast typeInfo typesNodesCount ns $ genIntervals nb $ fromIntegral typesNodesCount/nb
     
  where
    -- typeinfo -> potential -> current -> (desired min , desired max) -> generator -> types
    sampleOne :: RandomGen g => [TypeInfo] -> Int -> Int -> (Int,Int) -> g -> [(SourcePos,Type)]
    sampleOne [] _ _ _ _ = []
    sampleOne (ti:tis) p c (mn,mx) g =
      let n       = typeNodesCount ti
          p'      = p-n 
          (c',g') = randomR (max 0 (mn-p'-c),min n (mx-c)) g
          sc      = elemIndices c' $ typeNodesCountLattice ti
      in if null sc || p <= 0
         then []
         else let (xi,g'') = randomR (0,length sc-1) g'
              in (typePos ti,typeLattice ti !! (sc !! xi)):sampleOne tis p' (c'+c) (mn,mx) g''
    
    -- program -> typeinfo -> max # nodes -> interval -> # samples -> programs
    sampleN :: RandomGen g => L1 -> [TypeInfo] -> Int -> Interval Int -> Int -> g -> [L1]
    sampleN _ _ _ _ 0 _ = []
    sampleN p typeInfo m i n g =
      let g' = (snd $ next g)
      in replaceTypes (fromList (sampleOne (shuffle' typeInfo (length typeInfo) g') m 0 (inf i,sup i) g')) p:sampleN p typeInfo m i (n-1) g'

    sampleMN :: L1 -> [TypeInfo] -> Int -> Int -> [Interval Int] -> [[L1]]
    sampleMN _ _ _ _ [] = [[]]
    sampleMN p typeInfo m n (i:is) =  sampleN p typeInfo m i n (seedTFGen (0, 11, 22, 33)):sampleMN p typeInfo m n is
