{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Dynamizer.Sampling where

import           Control.Arrow                ((&&&))
import           Control.Monad                (foldM, guard, mzero)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Maybe    (MaybeT (..))
import qualified Data.DList                   as DL
import           Data.Function                (on)
import           Data.List                    (foldl', nub, sortBy, transpose,
                                               zipWith4, zipWith5)
import qualified Data.Map.Strict              as M
import           Data.Maybe                   (fromMaybe)
import           Data.Monoid                  (Sum (..))
import           Data.Random                  ()
import           Data.Random.RVar             (RVar, runRVar)
import           Data.Random.Shuffle.Weighted (weightedShuffle)
import           Numeric.Interval             (Interval, inf, interval, sup)
import           System.Random                (Random, RandomGen (..), randomR,
                                               randomRs)
import           System.Random.MWC            (withSystemRandom)
import           System.Random.Shuffle        (shuffle')
import           System.Random.TF             (seedTFGen)

import           Language.Grift.Source.Syntax
import           Language.Grift.Source.Utils

import           Dynamizer.Lattice


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
    tns = map (getSum . getSnd) typeinfo
    tps = map getFst typeinfo
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
               => Ann a (ExpF (Ann a Type))        -- ^ The fully-statically typed AST to sample from
               -> Int                              -- ^ The number of samples in each bin
               -> Double                           -- ^ The number of bins
               -> IO [[Ann a (ExpF (Ann a Type))]] -- ^ The list of bins, where each is a list of samples
sampleFromBins ast ns nb = do
  let (typeInfo, typesNodesCount) = genLatticeInfo ast
  withSystemRandom @IO $ runRVar $ mapM (sampleMany ns ast typeInfo typesNodesCount) $ genIntervals nb $ fromIntegral typesNodesCount/nb

sampleMany :: forall a. Ord a
           => Int
           -> Ann a (ExpF (Ann a Type))
           -> [Ann (a, Sum Int) Type]
           -> Int -- ^ total number of nodes
           -> Interval Int
           -> RVar [Ann a (ExpF (Ann a Type))]
sampleMany n p l m i =
  mapM (f . seedTFGen) $ take n $ zipWith4 (,,,) [0..] [11..] [22..] [33..]
  where
    f (split -> (g1, g2)) = do
      sl <- sampleOne (m, shuffle' l (length l) g1) i g2
      return $ replaceTypes (M.fromList sl) p

sampleOne :: forall a g. RandomGen g
          => (Int, [Ann (a, Sum Int) Type])
          -> Interval Int
          -> g
          -> RVar [(a, Ann a Type)]
sampleOne (p, ts) (inf &&& sup -> (mn, mx)) g = fst <$> foldM f ([], (p, 0, g)) ts
  where
    f :: ([(a, Ann a Type)], (Int, Int, g))
      -> Ann (a, Sum Int) Type
      -> RVar ([(a, Ann a Type)], (Int, Int, g))
    f x@(l, (potential, used, gen)) ct@(Ann (a, Sum c) _) =
      let potential' = potential-c
          (c', gen') = randomR (max 0 (mn-potential'-used), min c (mx-used)) gen
      in do st <- sampleLessPreciseType ct c'
            return $ if p <= 0 then x else case st of
              Nothing -> (l, (potential', used, gen'))
              Just t  -> ((a, t):l, (potential', used+c', gen'))

sampleLessPreciseType :: forall a. Ann (a, Sum Int) Type
                      -> Int
                      -> RVar (Maybe (Ann a Type))
sampleLessPreciseType s = runMaybeT . f s
  where
    f :: Ann (a, Sum Int) Type -> Int -> MaybeT RVar (Ann a Type)
    f (Ann (a, _) Dyn)          n | n == 0    = return $ Ann a Dyn
                                  | otherwise = mzero
    f (Ann (a, _) BlankTy)      n | n == 0    = return $ Ann a BlankTy
                                  | otherwise = mzero
    f (Ann _ (ArrTy _ _))       0 = mzero
    f t@(Ann (a, Sum n') _)     n | n == n' = return $ stripSnd t
                                  | n == 0  = return $ Ann a Dyn
    f (Ann (a, _) CharTy)       n | n == 1    = return $ Ann a CharTy
                                  | otherwise = mzero
    f (Ann (a, _) IntTy)        n | n == 1    = return $ Ann a IntTy
                                  | otherwise = mzero
    f (Ann (a, _) FloatTy)      n | n == 1    = return $ Ann a FloatTy
                                  | otherwise = mzero
    f (Ann (a, _) BoolTy)       n | n == 1    = return $ Ann a BoolTy
                                  | otherwise = mzero
    f (Ann (a, _) UnitTy)       n | n == 1    = return $ Ann a UnitTy
                                  | otherwise = mzero
    f (Ann (a, _) (RefTy t))    n = (Ann a . RefTy) <$> f t (n-1)
    f (Ann (a, _) (GRefTy t))   n = (Ann a . GRefTy) <$> f t (n-1)
    f (Ann (a, _) (MRefTy t))   n = (Ann a . MRefTy) <$> f t (n-1)
    f (Ann (a, _) (VectTy t))   n = (Ann a . VectTy) <$> f t (n-1)
    f (Ann (a, _) (GVectTy t))  n = (Ann a . GVectTy) <$> f t (n-1)
    f (Ann (a, _) (MVectTy t))  n = (Ann a . MVectTy) <$> f t (n-1)
    f (Ann (a, c) (FunTy ts t)) n = do
      (rt':ts') <- sampleTypeList (n-1) (c-1) (t:ts)
      return $ Ann a $ FunTy ts' rt'
    f (Ann (a, c) (ArrTy ts t)) n = do
      (rt':ts') <- sampleTypeList n c (t:ts)
      return $ Ann a $ ArrTy ts' rt'
    f (Ann (a, c) (TupleTy ts)) n = Ann a . TupleTy <$> sampleTypeList (n-1) (c-1) ts

    sampleTypeList :: Int -> Sum Int -> [Ann (a, Sum Int) Type] -> MaybeT RVar [Ann a Type]
    sampleTypeList n (Sum p) ts = do
      guard $ n <= p
      l <- lift $ weightedShuffle $ map (\y@(_, Ann (_, Sum x) _) -> (x, y)) $ zip [0..] ts
      mapM snd $ sortBy (compare `on` fst) $ fst $ foldl' g ([], (p, 0, seedTFGen (0, 11, 22, 33))) l
        where
          g :: forall g. RandomGen g
            => ([(Int, MaybeT RVar (Ann a Type))], (Int, Int, g))
            -> (Int, Ann (a, Sum Int) Type)
            -> ([(Int, MaybeT RVar (Ann a Type))], (Int, Int, g))
          g (l', (potential, used, gen)) (i, t@(Ann (_, Sum c) _)) =
            let potential' = potential-c
                (c', gen') = randomR (max 0 $ n-potential'-used,min c $ n-used) gen
            in ((i, f t c'):l', (potential', used+c', gen'))
