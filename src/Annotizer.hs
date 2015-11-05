{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}


module Annotizer where

import L1
  
class Gradual p where
  -- Generates the lattice of all possible gradually-typed versions.
  lattice :: p -> [p]
  -- Counts the number of less percise programs and the number of
  -- all type constructors
  count   :: p -> (Integer,Int)
  -- computes the percentage of dynamic code.
  dynamic :: Int -> p -> Double
  dynamic a e =
    if a > 0
    then fromIntegral (a - static e) / fromIntegral a
    else 0
  -- computes the number of type constructors.
  static  :: p -> Int

instance Gradual L1 where
  -- source information is not relevant
  lattice (Ann _ e)           = map (Ann undefined) $ lattice e
  count (Ann _ e)             = count e
  static (Ann _ e)            = static e

instance Gradual e => Gradual (ExpF e) where
  lattice (Op op es)          = Op op <$> mapM lattice es
  lattice (If e1 e2 e3)       = If <$> lattice e1 <*> lattice e2 <*> lattice e3
  lattice (App e1 es)         = App <$> lattice e1 <*> mapM lattice es 
  lattice (Lam args e t)      = (Lam args) <$> lattice e <*> lattice t
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

  count (Op _ es)             = let c = map count es
                                in (product $ map fst c, sum $ map snd c)
  count (If e1 e2 e3)         = let c1 = count e1
                                    c2 = count e2
                                    c3 = count e3
                                in (fst c1 * fst c2 * fst c3,
                                    snd c1 + snd c2 + snd c3)
  count (App e1 es)           = let c1 = count e1
                                    c = map count es
                                in (fst c1 * product (map fst c),
                                    snd c1 + sum (map snd c))
  count (Lam _ e t)           = let c1 = count e
                                    c2 = count t
                                in (fst c1 * fst c2,
                                    snd c1 + snd c2)
  count (GRef e)              = count e
  count (GDeRef e)            = count e
  count (GAssign e1 e2)       = let c1 = count e1
                                    c2 = count e2
                                in (fst c1 * fst c2,
                                    snd c1 + snd c2)
  count (MRef e)              = count e
  count (MDeRef e)            = count e
  count (MAssign e1 e2)       = let c1 = count e1
                                    c2 = count e2
                                in (fst c1 * fst c2,
                                    snd c1 + snd c2)
  count (GVect e1 e2)         = let c1 = count e1
                                    c2 = count e2
                                in (fst c1 * fst c2,
                                    snd c1 + snd c2)
  count (GVectRef e1 e2)      = let c1 = count e1
                                    c2 = count e2
                                in (fst c1 * fst c2,
                                    snd c1 + snd c2)
  count (GVectSet e1 e2 e3)   = let c1 = count e1
                                    c2 = count e2
                                    c3 = count e3
                                in (fst c1 * fst c2 * fst c3,
                                    snd c1 + snd c2 + snd c3)
  count (MVect e1 e2)         = let c1 = count e1
                                    c2 = count e2
                                in (fst c1 * fst c2,
                                    snd c1 + snd c2)
  count (MVectRef e1 e2)      = let c1 = count e1
                                    c2 = count e2
                                in (fst c1 * fst c2,
                                    snd c1 + snd c2)
  count (MVectSet e1 e2 e3)   = let c1 = count e1
                                    c2 = count e2
                                    c3 = count e3
                                in (fst c1 * fst c2 * fst c3,
                                    snd c1 + snd c2 + snd c3)
  count (Let e1 e2)           = let c1 = map count e1
                                    c2 = count e2
                                in (product (map fst c1) * fst c2,
                                    sum (map snd c1) + snd c2)
  count (Letrec e1 e2)        = let c1 = map count e1
                                    c2 = count e2
                                in (product (map fst c1) * fst c2,
                                    sum (map snd c1) + snd c2)
  count (As e t)              = let c1 = count e
                                    c2 = count t
                                in (fst c1 * fst c2,
                                    snd c1 + snd c2)
  count (Begin e' e)          = let c1 = map count e'
                                    c2 = count e
                                in (product (map fst c1) * fst c2,
                                    sum (map snd c1) + snd c2)
  count (Repeat _ e1 e2 e3)   = let c1 = count e1
                                    c2 = count e2
                                    c3 = count e3
                                in (fst c1 * fst c2 * fst c3,
                                    snd c1 + snd c2 + snd c3)
  count _                     = (1,0)

  static (Op _ es)           = sum (map static es)
  static (If e1 e2 e3)       = static e1 + static e2 + static e3
  static (App e1 es)         = static e1 + sum (map static es)
  static (Lam _ e t)         = static t + static e
  static (GRef e)            = static e
  static (GDeRef e)          = static e
  static (GAssign e1 e2)     = static e1 + static e2
  static (MRef e)            = static e
  static (MDeRef e)          = static e
  static (MAssign e1 e2)     = static e1 + static e2
  static (GVect e1 e2)       = static e1 + static e2
  static (GVectRef e1 e2)    = static e1 + static e2
  static (GVectSet e1 e2 e3) = static e1 + static e2 + static e3
  static (MVect e1 e2)       = static e1 + static e2
  static (MVectRef e1 e2)    = static e1 + static e2
  static (MVectSet e1 e2 e3) = static e1 + static e2 + static e3
  static (Let e1 e2)         = sum (map static e1) + static e2
  static (Letrec e1 e2)      = sum (map static e1) + static e2
  static (As e t)            = static t + static e
  static (Begin e' e)        = static e + sum (map static e')
  static (Repeat _ e1 e2 e3) = static e1 + static e2 + static e3
  static _                   = 0

instance Gradual e => Gradual (Bind e) where
  lattice (x,t,e) = (x,,) <$> lattice t <*> lattice e
  count (_,t,e)   =  let c1 = count e
                         c2 = count t
                     in (fst c1 * fst c2,
                         snd c1 + snd c2)
  static (_,t,e) = static e + static t

instance Gradual Type where
  lattice (GRefTy t)    = Dyn:(GRefTy <$> lattice t)
  lattice (MRefTy t)    = Dyn:(MRefTy <$> lattice t)
  lattice (GVectTy t)   = Dyn:(GVectTy <$> lattice t)
  lattice (MVectTy t)   = Dyn:(MVectTy <$> lattice t)
  lattice (FunTy t1 t2) = Dyn:(FunTy <$> mapM lattice t1 <*> lattice t2)
  lattice (ArrTy t1 t2) = ArrTy <$> mapM lattice t1 <*> lattice t2
  lattice Dyn           = [Dyn]
  lattice t             = [t,Dyn]

  count (GRefTy t)      = let c = count t in (fst c + 1, snd c + 1)
  count (MRefTy t)      = let c = count t in (fst c + 1, snd c + 1)
  count (GVectTy t)     = let c = count t in (fst c + 1, snd c + 1)
  count (MVectTy t)     = let c = count t in (fst c + 1, snd c + 1)
  count (FunTy t1 t2)   = let c1 = map count t1
                              c2 = count t2
                          in (1 + fst c2 * product (map fst c1),
                              1 + snd c2 + sum (map snd c1))
  count (ArrTy t1 t2)   = let c1 = map count t1
                              c2 = count t2
                          in (fst c2 * product (map fst c1),
                              snd c2 + sum (map snd c1))
  count _               = (2,1)

  static Dyn           = 0
  static (GRefTy t)    = 1 + static t
  static (MRefTy t)    = 1 + static t
  static (GVectTy t)   = 1 + static t
  static (MVectTy t)   = 1 + static t
  static (FunTy t1 t2) = 1 + sum (map static (t2:t1))
  static (ArrTy t1 t2) = sum (map static (t2:t1))
  static _             = 1
