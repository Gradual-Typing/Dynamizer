{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Sampling(
  sampleFromBins
  , sampleUniformally
  , sampleUniformally'
  ) where

import qualified Data.DList            as DL
import           Data.List             (nub, transpose, zipWith4, zipWith5)
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
sampleUniformally :: forall a. Ord a
                  => Ann a (ExpF (Ann a Type))   -- ^ The fully-statically typed AST to sample from
                  -> Int                         -- ^ The number of samples
                  -> [Ann a (ExpF (Ann a Type))] -- ^ The list of samples
sampleUniformally e ns = map (pick (embedLocalLattice e) . M.fromList . zip tps) $ nub rns
  where
    typeinfo = fst $ genLatticeInfo e
    tns = map getCount typeinfo
    tps = map getAnn typeinfo
    rns :: [[Int]]
    rns = transpose $ zipWith5 (\n a b c d -> randomList (0,n) ns $ seedTFGen (a,b,c,d)) tns [0..] [11..] [22..] [33..]

-- | Sample partially-typed versions uniformally. It generates the full lattice before sampling.
sampleUniformally' :: forall a t. Gradual (t (Ann a t))
                   => Ann a (ExpF (Ann a t))   -- ^ The fully-statically typed AST to sample from
                   -> Int                      -- ^ The number of samples
                   -> [Ann a (ExpF (Ann a t))] -- ^ The list of samples
sampleUniformally' e ns =
  let l = DL.toList $ lattice e
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
               => Ann a (ExpF (Ann a Type))     -- ^ The fully-statically typed AST to sample from
               -> Int                           -- ^ The number of samples in each bin
               -> Double                        -- ^ The number of bins
               -> [[Ann a (ExpF (Ann a Type))]] -- ^ The list of bins, where each is a list of samples
sampleFromBins ast ns nb =
  let (typeInfo, typesNodesCount) = genLatticeInfo ast
      intervals = genIntervals nb $ fromIntegral typesNodesCount/nb
  in map (take ns . sampleMany ast typeInfo typesNodesCount) intervals
  where
    sampleOne :: RandomGen g
              => Int -- ^ remaining
              -> Int -- ^ current
              -> Int -- ^ desired min
              -> Int -- ^ desired max
              -> g
              -> [Ann (Int, a) Type]
              -> [(a, Ann a Type)]
    sampleOne _ _ _ _ _ [] = []
    sampleOne p c mn mx g (ct@(Ann (n, a) _):tis) =
      let p'        = p-n
          (c', g')  = randomR (max 0 (mn-p'-c), min n (mx-c)) g
          sc        = genLessPreciseType c' ct
          (xi, g'') = randomR (0, length sc-1) g'
      in if | p <= 0    -> []
            | null sc   -> sampleOne p' c mn mx g' tis
            | otherwise -> (a, sc !! xi):sampleOne p' (c'+c) mn mx g'' tis

    sampleMany :: Ann a (ExpF (Ann a Type))
               -> [Ann (Int, a) Type]
               -> Int                         -- ^ total number of nodes
               -> Interval Int
               -> [Ann a (ExpF (Ann a Type))] -- ^ stream of samples
    sampleMany p l m i =
      map (f . seedTFGen) $ zipWith4 (,,,) [0..] [11..] [22..] [33..]
      where
        sampleOne' = sampleOne m 0 (inf i) (sup i)
        f (split -> (g1, g2)) =
          replaceTypes (M.fromList $ sampleOne' g2 $ shuffle' l (length l) g1) p
