{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Sampling(
  sampleFromBins,
  sampleUniformally,
  sampleUniformally'
  ) where

import Data.List (transpose,nub,zipWith5,elemIndices)
import System.Random (Random,RandomGen(..),randomR,randomRs)
import System.Random.TF (seedTFGen)
import System.Random.Shuffle(shuffle')
import Numeric.Interval (Interval,inf,sup,interval)
import Data.Maybe (fromMaybe)
import Control.Monad.State.Lazy (runState,evalState)
import qualified Data.Map.Strict as M

import L1
import Annotizer


randomList :: (Random a,RandomGen g) => (a,a) -> Int -> g -> [a]
randomList bnds n = take n . randomRs bnds

-- | Sample partially-typed versions uniformally.
sampleUniformally :: L1 -- ^ The fully-statically typed AST to sample from
                  -> Int  -- ^ The number of samples
                  -> [L1] -- ^ The list of samples
sampleUniformally e ns = map (pick (localLattice e) . M.fromList . zip tps) $ nub rns
  where    
    typeinfo = evalState (genTypeInfo undefined e) 0
    tns = map typeNodesCount typeinfo
    tps = map typePos typeinfo
    rns = transpose $ zipWith5 (\n a b c d -> randomList (0,n) ns $ seedTFGen (a,b,c,d)) tns [0..] [11..] [22..] [33..]

-- | Sample partially-typed versions uniformally. It generates the full lattice before sampling.
sampleUniformally' :: L1 -- ^ The fully-statically typed AST to sample from
                   -> Int  -- ^ The number of samples
                   -> [L1] -- ^ The list of samples
sampleUniformally' e ns =
  let l = lattice e
  in map (l !!) $ take ns $ nub $ randomRs (0,length l - 1) $ seedTFGen (0, 11, 22, 33)

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
      let (g1,g') = split g
          (g2,g3) = split g'
      in replaceTypes (M.fromList (sampleOne (shuffle' typeInfo (length typeInfo) g1) m 0 (inf i,sup i) g2)) p:sampleN p typeInfo m i (n-1) g3

    sampleMN :: L1 -> [TypeInfo] -> Int -> Int -> [Interval Int] -> [[L1]]
    sampleMN _ _ _ _ [] = [[]]
    sampleMN p typeInfo m n (i:is) =  sampleN p typeInfo m i n (seedTFGen (0, 11, 22, 33)):sampleMN p typeInfo m n is
