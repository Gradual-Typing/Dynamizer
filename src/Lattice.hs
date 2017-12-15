{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Lattice where

import           Control.Arrow      ((***))
import           Data.Bifoldable    (Bifoldable, bifoldMap)
import           Data.Bifunctor     (bimap)
import           Data.Bitraversable (bitraverse)
import qualified Data.DList         as DL
import qualified Data.Map.Strict    as M
import           Data.Maybe         (fromMaybe)
import           Data.Monoid        (Product (..), Sum (..))

import           Syntax

embedLocalLattice :: forall a t. Gradual (t (Ann a t))
                  => Ann a (ExpF (Ann a t))
                  -> Ann a (ExpF ([Ann a t], Ann a t))
embedLocalLattice (Ann s e) = Ann s $ bimap (\t -> (lattice t, t)) embedLocalLattice e

pick :: forall a t. (Gradual (t (Ann a t)), Ord a)
  => Ann a (ExpF ([Ann a t], Ann a t))
  -> M.Map a Int
  -> Ann a (ExpF (Ann a t))
pick (Ann s e) src2indx = Ann s $ bimap pick' (`pick` src2indx) e
  where
    pick' :: ([Ann a t], Ann a t) -> Ann a t
    pick' (ts, t@(Ann s' _)) = maybe t (ts !!) $ M.lookup s' src2indx

replaceTypes :: forall a. Ord a
  => M.Map a (Ann a Type)
  -> Ann a (ExpF (Ann a Type))
  -> Ann a (ExpF (Ann a Type))
replaceTypes src2ty (Ann s e) = Ann s $ bimap replaceTypes' (replaceTypes src2ty) e
  where
    dyn :: a -> Type (Ann a Type) -> Ann a Type
    dyn s' (ArrTy l _) = Ann s' $ ArrTy (replicate (length l) (Ann s' Dyn)) (Ann s' Dyn)
    dyn s' _           = Ann s' Dyn

    replaceTypes' :: Ann a Type -> Ann a Type
    replaceTypes' (Ann s' t) = fromMaybe (dyn s' t) $ M.lookup s' src2ty

class Gradual p where
  -- Generates the lattice of all possible gradually-typed versions.
  lattice :: p -> [p]
  -- Counts the number of less percise programs and the number of
  -- all type constructors
  count   :: p -> (Product Integer, Sum Int)
  -- computes the percentage of dynamic code.
  dynamic :: Int -> p -> Double
  dynamic a e =
    if a > 0
    then fromIntegral (a - getSum (static e)) / fromIntegral a
    else 0
  -- counts the number of static type nodes in a program
  static  :: p -> Sum Int

instance Gradual (e (Ann a e)) => Gradual (Ann a e) where
  lattice (Ann i e) = Ann i <$> lattice e
  count (Ann _ e)   = count e
  static (Ann _ e)  = static e

instance (Gradual t, Gradual e) => Gradual (ExpF t e) where
  lattice = bitraverse lattice pure
  count   = bifoldMap count count
  static  = bifoldMap static static

instance Gradual t => Gradual (Type t) where
  lattice (RefTy t)     = Dyn:(RefTy <$> lattice t)
  lattice (GRefTy t)    = Dyn:(GRefTy <$> lattice t)
  lattice (MRefTy t)    = Dyn:(MRefTy <$> lattice t)
  lattice (VectTy t)    = Dyn:(VectTy <$> lattice t)
  lattice (GVectTy t)   = Dyn:(GVectTy <$> lattice t)
  lattice (MVectTy t)   = Dyn:(MVectTy <$> lattice t)
  lattice (FunTy t1 t2) = Dyn:(FunTy <$> mapM lattice t1 <*> lattice t2)
  lattice (ArrTy t1 t2) = ArrTy <$> mapM lattice t1 <*> lattice t2
  lattice (TupleTy ts)  = Dyn:(TupleTy <$> mapM lattice ts)
  lattice Dyn           = [Dyn]
  lattice t             = [Dyn,t]

  count (RefTy t)     = ((+1) *** (+1)) $ count t
  count (GRefTy t)    = ((+1) *** (+1)) $ count t
  count (MRefTy t)    = ((+1) *** (+1)) $ count t
  count (VectTy t)    = ((+1) *** (+1)) $ count t
  count (GVectTy t)   = ((+1) *** (+1)) $ count t
  count (MVectTy t)   = ((+1) *** (+1)) $ count t
  count (FunTy t1 t2) = let c1 = map count t1
                            c2 = count t2
                        in (1 + fst c2 * product (map fst c1),
                            1 + snd c2 + sum (map snd c1))
  count (ArrTy t1 t2) = let c1 = map count t1
                            c2 = count t2
                        in (fst c2 * product (map fst c1),
                            snd c2 + sum (map snd c1))
  count (TupleTy ts)  = let c1 = map count ts
                        in (1 + product (map fst c1),
                            1 + sum (map snd c1))
  count Dyn           = (1, 1)
  count _             = (2, 1)

  static Dyn           = 0
  static (RefTy t)     = 1 + static t
  static (GRefTy t)    = 1 + static t
  static (MRefTy t)    = 1 + static t
  static (VectTy t)    = 1 + static t
  static (GVectTy t)   = 1 + static t
  static (MVectTy t)   = 1 + static t
  static (FunTy t1 t2) = 1 + sum (map static (t2:t1))
  static (ArrTy t1 t2) = sum (map static (t2:t1))
  static (TupleTy ts)  = 1 + sum (map static ts)
  static _             = 1

data LatticeInfo a t =  LatticeInfo { annotation        :: a
                                    , nodesCount        :: Int
                                    , nodesCountLattice :: [Int]
                                    , latticeList       :: [Ann a t]
                                    }

genLatticeInfo :: forall e t a. (Gradual (t (Ann a t)), Bifoldable e)
               => Ann a (e (Ann a t))
               -> ([LatticeInfo a t], Int)
genLatticeInfo = (DL.toList *** getSum) . localLattice
  where
    localLattice :: Ann a (e (Ann a t)) -> (DL.DList (LatticeInfo a t), Sum Int)
    localLattice (Ann _ e) = bifoldMap f localLattice e

    f :: Ann a t -> (DL.DList (LatticeInfo a t), Sum Int)
    f t@(Ann a _) =
      let ts = lattice t
          nodesCount' = static t
      in (DL.singleton (LatticeInfo a (getSum nodesCount') (map (getSum . static) ts) ts), nodesCount')
