{-# LANGUAGE UndecidableInstances #-}

module Test.Lattice where

import           Data.Bifoldable       (bifoldMap)
import           Data.Monoid           (Sum (..))

import           Language.Grift.Source.Syntax

class Gradual p where
  topLvlFunCount :: p -> Sum Int

instance Gradual (e (Ann a e)) => Gradual (Ann a e) where
  topLvlFunCount (Ann _ e) = topLvlFunCount e

instance (Gradual t, Gradual e) => Gradual (ExpF t e) where
  topLvlFunCount (DLam _ _ _ _) = Sum 1
  topLvlFunCount (Lam _ _ _) = Sum 1
  topLvlFunCount e = bifoldMap (const mempty) topLvlFunCount e

instance Gradual t => Gradual (Type t) where
  topLvlFunCount = mempty
