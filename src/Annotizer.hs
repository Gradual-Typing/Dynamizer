{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Annotizer where

import L1

class Gradual p where
  lattice :: p -> [p]
  count   :: p -> Integer

instance Gradual L1 where
  lattice (s,e) = map (s,) $ lattice e
  count (_,e) = count e
  
instance Gradual Exp where
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

instance Gradual Arg where
  lattice (x,t) = (x,) <$> lattice t
  count (_,t) = count t

instance Gradual Bind where
  lattice (x,t,e) =  (x,,) <$> lattice t <*> lattice e
  count (_,t,e) =  count t * count e

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
