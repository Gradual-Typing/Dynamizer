{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Sampling(
  sampleFromBins
  , sampleUniformally
  , sampleUniformally'
  ) where

import           Data.List             (elemIndices, nub, transpose, zipWith5)
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromMaybe)
import           Numeric.Interval      (Interval, inf, interval, sup)
import           System.Random         (Random, RandomGen (..), randomR,
                                        randomRs)
import           System.Random.Shuffle (shuffle')
import           System.Random.TF      (seedTFGen)

import           Lattice
import           Syntax

randomList :: (Random a, RandomGen g) => (a,a) -> Int -> g -> [a]
randomList bnds n = take n . randomRs bnds

-- | Sample partially-typed versions uniformally.
sampleUniformally :: forall a t. (Gradual (t (Ann a t)), Ord a)
                  => Ann a (ExpF (Ann a t)) -- ^ The fully-statically typed AST to sample from
                  -> Int  -- ^ The number of samples
                  -> [Ann a (ExpF (Ann a t))] -- ^ The list of samples
sampleUniformally e ns = map (pick (embedLocalLattice e) . M.fromList . zip tps) $ nub rns
  where
    typeinfo = fst $ genLatticeInfo e
    tns = map nodesCount typeinfo
    tps = map annotation typeinfo
    rns :: [[Int]]
    rns = transpose $ zipWith5 (\n a b c d -> randomList (0,n) ns $ seedTFGen (a,b,c,d)) tns [0..] [11..] [22..] [33..]

-- | Sample partially-typed versions uniformally. It generates the full lattice before sampling.
sampleUniformally' :: forall a t. Gradual (t (Ann a t))
                   => Ann a (ExpF (Ann a t)) -- ^ The fully-statically typed AST to sample from
                   -> Int  -- ^ The number of samples
                   -> [Ann a (ExpF (Ann a t))] -- ^ The list of samples
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
sampleFromBins :: forall a. Ord a
               => Ann a (ExpF (Ann a Type)) -- ^ The fully-statically typed AST to sample from
               -> Int    -- ^ The number of samples in each bin
               -> Double -- ^ The number of bins
               -> [[Ann a (ExpF (Ann a Type))]] -- ^ The list of bins, where each is a list of samples
sampleFromBins ast ns nb =
  let (typeInfo, typesNodesCount) = genLatticeInfo ast
      intervals = genIntervals nb $ fromIntegral typesNodesCount/nb
  in map (take ns . sampleMany ast typeInfo typesNodesCount) intervals
  where
    sampleOne :: RandomGen g
      => [LatticeInfo a Type]
      -> Int -- ^ potential
      -> Int -- ^ current
      -> Int -- ^ desired min
      -> Int -- ^ desired max
      -> g
      -> [(a, Ann a Type)]
    sampleOne [] _ _ _ _ _ = []
    sampleOne (ti:tis) p c mn mx g =
      let n       = nodesCount ti
          p'      = p-n
          (c',g') = randomR (max 0 (mn-p'-c),min n (mx-c)) g
          sc      = elemIndices c' $ nodesCountLattice ti
      in if null sc || p <= 0
         then []
         else let (xi,g'') = randomR (0,length sc-1) g'
              in (annotation ti,latticeList ti !! (sc !! xi)):sampleOne tis p' (c'+c) mn mx g''

    sampleMany :: Ann a (ExpF (Ann a Type))
               -> [LatticeInfo a Type]
               -> Int -- ^ total number of nodes
               -> Interval Int
               -> [Ann a (ExpF (Ann a Type))]
    sampleMany p ll m i = map f seeds
      where
        seeds = seedTFGen (0, 11, 22, 33):seeds
        f g =
          let (g1, g2) = split g
              ll'      = shuffle' ll (length ll) g1
          in replaceTypes (M.fromList $ sampleOne ll' m 0 (inf i) (sup i) g2) p
