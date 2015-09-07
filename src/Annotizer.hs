{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Annotizer where

import L1
  
class Gradual p where
  lattice :: p -> [p]
  count   :: p -> Integer
  dynamic :: p -> Double -> Double -> (Double,Double)
  static  :: p -> Double
  static e = let (dyns,alls) = dynamic e 0 0 in (alls - dyns) / alls

instance Gradual L1 where
  lattice (Ann _ e) = map (Ann undefined) $ lattice e -- source information is not relevant
  count (Ann _ e) = count e
  dynamic (Ann _ e) = dynamic e

instance Gradual e => Gradual (ExpF e) where
  lattice (Op op es) = Op op <$> mapM lattice es
  lattice (If e1 e2 e3) = If <$> lattice e1 <*> lattice e2 <*> lattice e3
  lattice (App e1 es) = App <$> lattice e1 <*> mapM lattice es 
  lattice (Lam args e t) = Lam <$> mapM lattice args <*> lattice e <*> lattice t
  lattice (GRef e) = GRef <$> lattice e
  lattice (GDeRef e) = GDeRef <$> lattice e
  lattice (GAssign e1 e2) = GAssign <$> lattice e1 <*> lattice e2
  lattice (MRef e) = MRef <$> lattice e
  lattice (MDeRef e) = MDeRef <$> lattice e
  lattice (MAssign e1 e2) = MAssign <$> lattice e1 <*> lattice e2
  lattice (GVect e1 e2) = GVect <$> lattice e1 <*> lattice e2
  lattice (GVectRef e1 e2) = GVectRef <$> lattice e1 <*> lattice e2
  lattice (GVectSet e1 e2 e3) = GVectSet <$> lattice e1 <*> lattice e2 <*> lattice e3
  lattice (MVect e1 e2) = MVect <$> lattice e1 <*> lattice e2
  lattice (MVectRef e1 e2) = MVectRef <$> lattice e1 <*> lattice e2
  lattice (MVectSet e1 e2 e3) = MVectSet <$> lattice e1 <*> lattice e2 <*> lattice e3
  lattice (Let e1 e2) = Let <$> mapM lattice e1 <*> lattice e2
  lattice (Letrec e1 e2) = Letrec <$> mapM lattice e1 <*> lattice e2
  lattice (As e t) = As <$> lattice e <*> lattice t
  lattice (Begin e' e) = Begin <$> mapM lattice e' <*> lattice e
  lattice (Repeat i e1 e2 e) = Repeat i <$> lattice e1 <*> lattice e2 <*> lattice e
  lattice e = [e]

  count (Op _ es) = product $ map count es
  count (If e1 e2 e3) = count e1 * count e2 * count e3
  count (App e1 es) = count e1* product (map count es)
  count (Lam args e t) = product (map count args) * count e * count t
  count (GRef e) = count e
  count (GDeRef e) = count e
  count (GAssign e1 e2) = count e1 * count e2
  count (MRef e) = count e
  count (MDeRef e) = count e
  count (MAssign e1 e2) = count e1 * count e2
  count (GVect e1 e2) = count e1 * count e2
  count (GVectRef e1 e2) = count e1 * count e2
  count (GVectSet e1 e2 e3) = count e1 * count e2 * count e3
  count (MVect e1 e2) = count e1 * count e2
  count (MVectRef e1 e2) = count e1 * count e2
  count (MVectSet e1 e2 e3) = count e1 * count e2 * count e3
  count (Let e1 e2) = product (map count e1) * count e2
  count (Letrec e1 e2) = product (map count e1) * count e2
  count (As e t) = count e * count t
  count (Begin e' e) = product (map count e') * count e
  count (Repeat _ e1 e2 e) = count e1 * count e2 * count e
  count _ = 1

  dynamic (Op _ es) a1 a2 = foldP es a1 a2
  dynamic (If e1 e2 e3) a1 a2 = foldP [e1,e2,e3] a1 a2
  dynamic (App e1 es) a1 a2 = foldP (e1:es) a1 a2
  dynamic (Lam args e t) a1 a2 = let (a1',a2') = foldP (t:map snd args) a1 a2 in dynamic e a1' a2'
  dynamic (GRef e) a1 a2 = dynamic e a1 a2
  dynamic (GDeRef e) a1 a2 = dynamic e a1 a2
  dynamic (GAssign e1 e2) a1 a2 = foldP [e1,e2] a1 a2
  dynamic (MRef e) a1 a2 = dynamic e a1 a2
  dynamic (MDeRef e) a1 a2 = dynamic e a1 a2
  dynamic (MAssign e1 e2) a1 a2 = foldP [e1,e2] a1 a2
  dynamic (GVect e1 e2) a1 a2 = foldP [e1,e2] a1 a2
  dynamic (GVectRef e1 e2) a1 a2 = foldP [e1,e2] a1 a2
  dynamic (GVectSet e1 e2 e3) a1 a2 = foldP [e1,e2,e3] a1 a2
  dynamic (MVect e1 e2) a1 a2 = foldP [e1,e2] a1 a2
  dynamic (MVectRef e1 e2) a1 a2 = foldP [e1,e2] a1 a2
  dynamic (MVectSet e1 e2 e3) a1 a2 = foldP [e1,e2,e3] a1 a2
  dynamic (Let e1 e2) a1 a2 = let (a1',a2') = foldP e1 a1 a2 in dynamic e2 a1' a2'
  dynamic (Letrec e1 e2) a1 a2 = let (a1',a2') = foldP e1 a1 a2 in dynamic e2 a1' a2'
  dynamic (As e t) a1 a2 = let (a1',a2') = dynamic e a1 a2 in dynamic t a1' a2'
  dynamic (Begin e' e) a1 a2 = let (a1',a2') = foldP e' a1 a2 in dynamic e a1' a2'
  dynamic (Repeat _ e1 e2 e) a1 a2  = foldP [e1,e2,e] a1 a2
  dynamic _ a1 a2 = (a1,a2)

foldP :: Gradual a => [a] -> Double -> Double -> (Double,Double)
foldP es dyns alls = foldl (\(b1,b2) a -> dynamic a b1 b2) (dyns,alls) es

instance Gradual Arg where
  lattice (x,t) = (x,) <$> lattice t
  count (_,t) = count t
  dynamic (_,Dyn) a1 a2 = (a1+1,a2+1)
  dynamic _ a1 a2 = (a1,a2+1)

instance Gradual e => Gradual (Bind e) where
  lattice (x,t,e) =  (x,,) <$> lattice t <*> lattice e
  count (_,t,e) =  count t * count e
  dynamic (_,Dyn,e) a1 a2 = dynamic e (a1+1) (a2+1)
  dynamic (_,_,e) a1 a2 = dynamic e a1 (a2+1)

instance {-# OVERLAPPABLE #-} Gradual e => Gradual (UBind e) where
  lattice (x,e) =  (x,) <$> lattice e
  count (_,e) = count e
  dynamic (_,e) a1 a2 = dynamic e a1 (a2+1)

instance Gradual Type where
  lattice (GRefTy t) = Dyn: (GRefTy <$> lattice t)
  lattice (MRefTy t) = Dyn: (MRefTy <$> lattice t)
  lattice (GVectTy t) = Dyn: (GVectTy <$> lattice t)
  lattice (MVectTy t) = Dyn: (MVectTy <$> lattice t)
  lattice (FunTy t1 t2) = Dyn: (FunTy <$> mapM lattice t1 <*> lattice t2)
  lattice t = [t,Dyn]

  count (GRefTy t) = 2 * count t
  count (MRefTy t) = 2 * count t
  count (GVectTy t) = 2 * count t
  count (MVectTy t) = 2 * count t
  count (FunTy t1 t2) = 2 * product (map count t1) * count t2
  count _ = 2

  dynamic Dyn a1 a2 = (a1+1,a2+1)
  dynamic _ a1 a2 = (a1,a2+1)
