{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Main where

import           Control.Arrow             ((&&&))
import           Control.Monad             (zipWithM)
import           Control.Monad.Trans.Class (lift)
import           Data.List                 (zipWith4)
import           Data.Monoid               (Sum (..))
import           Generic.Random
import           GHC.Generics
import           Numeric.Interval          (Interval, member)
import           System.Random.TF          (seedTFGen)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           Language.Grift.Source.Syntax
import           Language.Grift.Source.Utils

import           Dynamizer.Lattice
import           Dynamizer.Sampling

import           Test.Sampling


deriving instance Generic (Ann a Type)
deriving instance Generic (Type a)
deriving instance Generic (Ann a (ExpF (Ann a Type)))
deriving instance Generic (ExpF (Ann a Type) (Ann a (ExpF (Ann a Type))))
deriving instance Generic Prim
deriving instance Generic Operator

instance Arbitrary Prim where
  arbitrary = genericArbitraryU

instance Arbitrary Operator where
  arbitrary = genericArbitraryU

instance (Arbitrary a, BaseCase (Ann a Type)) => Arbitrary (Ann a Type) where
  arbitrary = genericArbitraryU

instance (Arbitrary a, BaseCase (Type a)) => Arbitrary (Type a) where
  arbitrary = genericArbitrary' (1 % 1 % 10 % 10 % 10 % 10 % 10 % 10 % 10 % 10 % 10 % 10 % 10 % 10 % 10 % 10 % 10 % 10 % ())

instance (Arbitrary a
         , BaseCase (Ann a Type)
         , BaseCase (ExpF (Ann a Type) (Ann a (ExpF (Ann a Type))))
         , BaseCase (Ann a (ExpF (Ann a Type)))) => Arbitrary (Ann a (ExpF (Ann a Type))) where
  arbitrary = genericArbitraryU

instance (Arbitrary a
         , BaseCase (Ann a (ExpF (Ann a Type)))
         , BaseCase (Ann a Type)
         , BaseCase (ExpF (Ann a Type) (Ann a (ExpF (Ann a Type))))) => Arbitrary (ExpF (Ann a Type) (Ann a (ExpF (Ann a Type)))) where
  arbitrary = genericArbitrary' (10 % 10 % 10 % 10 % 10 % 10 % 10 % 10 % 10 % 10 % 10 % 10 % 10 % 10 % 10 % 10 % 10 % 10 % 10 % 10 % 10 % 10 % 10 % 10 % 10 % 10 % 10 % 10 % 10 % 10 % 10 % 10 % 10 % 10 % 10 % ())

prop_sampleLessPreciseType :: Ann () Type -> NonNegative Int -> Property
prop_sampleLessPreciseType t (NonNegative s) = monadicIO $ do
  maybeType <- lift $ sampleLessPreciseTypeIO t s
  case maybeType of
    Just ty -> assert (static' ty == s)
    Nothing -> assert True

test_sampleOne :: [Ann () Type] -> Int -> IO ([[Ann () Type]], [Interval Int])
test_sampleOne ts  nb =
  let ts' = map annotateTypeWithCount ts
      p = getSum $ sum $ map getSnd ts'
      is = genIntervals (fromIntegral nb) $ fromIntegral p/fromIntegral nb
      seeds = map seedTFGen $ zipWith4 (,,,) [0..] [11..] [22..] [3..]
  in do z <- zipWithM (sampleOneIO (p, ts')) is seeds
        return (z, is)

prop_sampleOne :: [Ann () Type] -> Property
prop_sampleOne ts =
  forAll (choose (1, 20)) $ \nb ->
    monadicIO $ do
    (r, is) <- lift $ test_sampleOne ts nb
    let (r', is') = (map fst &&& map snd) $ filter (not . null . fst) $  zip r is
    assert $ and $ zipWith (member . sum . map static') r' is'

prop_funLattice :: Ann () (ExpF (Ann () Type)) -> Property
prop_funLattice p =
  monadicIO $ assert $ length (funLattice p) == 2 ^ getSum (funCount p)

main :: IO ()
main = do
  quickCheckWith stdArgs{maxSize=18, maxSuccess=200} prop_funLattice
  quickCheckWith stdArgs{maxSize=15, maxSuccess=200} prop_sampleLessPreciseType
  quickCheck prop_sampleOne
