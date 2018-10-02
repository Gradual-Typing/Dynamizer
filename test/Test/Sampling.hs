{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}

module Test.Sampling where

import           Data.Monoid           (Sum (..))
import           Data.Random.RVar      (runRVar)
import           Numeric.Interval      (Interval)
import           System.Random         (RandomGen (..))
import           System.Random.MWC     (withSystemRandom)

import           Language.Grift.Source.Syntax

import           Dynamizer.Lattice
import           Dynamizer.Sampling

sampleLessPreciseTypeIO :: forall a. Ann a Type
                        -> Int
                        -> IO (Maybe (Ann a Type))
sampleLessPreciseTypeIO t s = withSystemRandom @IO $ runRVar $ sampleLessPreciseType (annotateTypeWithCount t) s

sampleOneIO :: forall a g. RandomGen g
            => (Int, [Ann (a, Sum Int) Type])
            -> Interval Int
            -> g
            -> IO [Ann a Type]
sampleOneIO p i g = map snd <$> withSystemRandom @IO (runRVar $ sampleOne p i g)

static' :: Gradual a => a -> Int
static' = getSum . static
