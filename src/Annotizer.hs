{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}


module Annotizer where

import Control.Arrow((***))
import Control.Monad(join)
import Data.List(foldl')

import L1
  
class Gradual p where
  -- Generates the lattice of all possible gradually-typed versions.
  lattice :: p -> [p]
  -- Counts the number of all possible gradually-typed versions.
  -- The total number of all variants should be equal to
  -- 2^(number of all annotations) + number of complex types
  count   :: p -> Integer
  -- Counts the number of both all dynamic types and all types.
  dynamic :: p -> (Int,Int) -> (Int,Int)
  -- Counts the number of all static types.
  static  :: p -> Double
  static e = let (dyns,alls) = dynamic e (0,0)
             in if alls > 0
                then fromIntegral (alls - dyns) / fromIntegral alls
                else 0

instance Gradual L1 where
  -- source information is not relevant
  lattice (Ann _ e)           = map (Ann undefined) $ lattice e
  count (Ann _ e)             = count e
  dynamic (Ann _ e)           = dynamic e

instance Gradual e => Gradual (ExpF e) where
  lattice (Op op es)          = Op op <$> mapM lattice es
  lattice (If e1 e2 e3)       = If <$> lattice e1 <*> lattice e2 <*> lattice e3
  lattice (App e1 es)         = App <$> lattice e1 <*> mapM lattice es 
  lattice (Lam args e)        = (Lam args) <$> lattice e
  lattice (GRef e)            = GRef <$> lattice e
  lattice (GDeRef e)          = GDeRef <$> lattice e
  lattice (GAssign e1 e2)     = GAssign <$> lattice e1 <*> lattice e2
  lattice (MRef e)            = MRef <$> lattice e
  lattice (MDeRef e)          = MDeRef <$> lattice e
  lattice (MAssign e1 e2)     = MAssign <$> lattice e1 <*> lattice e2
  lattice (GVect e1 e2)       = GVect <$> lattice e1 <*> lattice e2
  lattice (GVectRef e1 e2)    = GVectRef <$> lattice e1 <*> lattice e2
  lattice (GVectSet e1 e2 e3) = GVectSet <$> lattice e1 <*> lattice e2
                                <*> lattice e3
  lattice (MVect e1 e2)       = MVect <$> lattice e1 <*> lattice e2
  lattice (MVectRef e1 e2)    = MVectRef <$> lattice e1 <*> lattice e2
  lattice (MVectSet e1 e2 e3) = MVectSet <$> lattice e1 <*> lattice e2
                                <*> lattice e3
  lattice (Let e1 e2)         = Let <$> mapM lattice e1 <*> lattice e2
  lattice (Letrec e1 e2)      = Letrec <$> mapM lattice e1 <*> lattice e2
  lattice (As e t)            = As <$> lattice e <*> lattice t
  lattice (Begin e' e)        = Begin <$> mapM lattice e' <*> lattice e
  lattice (Repeat i e1 e2 e)  = Repeat i <$> lattice e1 <*> lattice e2
                                <*> lattice e
  lattice e                   = [e]

  count (Op _ es)             = product $ map count es
  count (If e1 e2 e3)         = count e1 * count e2 * count e3
  count (App e1 es)           = count e1* product (map count es)
  count (Lam _ e)             = count e
  count (GRef e)              = count e
  count (GDeRef e)            = count e
  count (GAssign e1 e2)       = count e1 * count e2
  count (MRef e)              = count e
  count (MDeRef e)            = count e
  count (MAssign e1 e2)       = count e1 * count e2
  count (GVect e1 e2)         = count e1 * count e2
  count (GVectRef e1 e2)      = count e1 * count e2
  count (GVectSet e1 e2 e3)   = count e1 * count e2 * count e3
  count (MVect e1 e2)         = count e1 * count e2
  count (MVectRef e1 e2)      = count e1 * count e2
  count (MVectSet e1 e2 e3)   = count e1 * count e2 * count e3
  count (Let e1 e2)           = product (map count e1) * count e2
  count (Letrec e1 e2)        = product (map count e1) * count e2
  count (As e t)              = count e * count t
  count (Begin e' e)          = product (map count e') * count e
  count (Repeat _ e1 e2 e)    = count e1 * count e2 * count e
  count _                     = 1

  dynamic (Op _ es)           = foldP es
  dynamic (If e1 e2 e3)       = foldP [e1,e2,e3]
  dynamic (App e1 es)         = foldP (e1:es)
  dynamic (Lam _ e)           = dynamic e
  dynamic (GRef e)            = dynamic e
  dynamic (GDeRef e)          = dynamic e
  dynamic (GAssign e1 e2)     = foldP [e1,e2]
  dynamic (MRef e)            = dynamic e
  dynamic (MDeRef e)          = dynamic e
  dynamic (MAssign e1 e2)     = foldP [e1,e2]
  dynamic (GVect e1 e2)       = foldP [e1,e2]
  dynamic (GVectRef e1 e2)    = foldP [e1,e2]
  dynamic (GVectSet e1 e2 e3) = foldP [e1,e2,e3]
  dynamic (MVect e1 e2)       = foldP [e1,e2]
  dynamic (MVectRef e1 e2)    = foldP [e1,e2]
  dynamic (MVectSet e1 e2 e3) = foldP [e1,e2,e3]
  dynamic (Let e1 e2)         = dynamic e2 . foldP e1
  dynamic (Letrec e1 e2)      = dynamic e2 . foldP e1
  dynamic (As e t)            = dynamic t . dynamic e
  dynamic (Begin e' e)        = dynamic e . foldP e'
  dynamic (Repeat _ e1 e2 e)  = foldP [e1,e2,e]
  dynamic _                   = id

foldP :: Gradual a => [a] -> (Int,Int) -> (Int,Int)
foldP = flip $ foldl' $ flip dynamic

instance Gradual e => Gradual (Bind e) where
  lattice (x,t,e) = (x,,) <$> lattice t <*> lattice e
  count (_,t,e)   =  count t * count e
  dynamic (_,t,e) = dynamic e . dynamic t

instance Gradual Type where
  lattice (GRefTy t)    = Dyn:(GRefTy <$> lattice t)
  lattice (MRefTy t)    = Dyn:(MRefTy <$> lattice t)
  lattice (GVectTy t)   = Dyn:(GVectTy <$> lattice t)
  lattice (MVectTy t)   = Dyn:(MVectTy <$> lattice t)
  lattice (FunTy t1 t2) = Dyn:(FunTy <$> mapM lattice t1 <*> lattice t2)
  lattice Dyn           = [Dyn]
  lattice t             = [t,Dyn]

  count (GRefTy t)      = count t + 1
  count (MRefTy t)      = count t + 1
  count (GVectTy t)     = count t + 1
  count (MVectTy t)     = count t + 1
  count (FunTy t1 t2)   = (product (map count t1) * count t2) + 1
  count _               = 2

  dynamic Dyn           = join (***) (+1)
  dynamic (GRefTy t)    = dynamic t
  dynamic (MRefTy t)    = dynamic t
  dynamic (GVectTy t)   = dynamic t
  dynamic (MVectTy t)   = dynamic t
  dynamic (FunTy t1 t2) = foldP (t2:t1)
  dynamic _             = id *** (+1)
