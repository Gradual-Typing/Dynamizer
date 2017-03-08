{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}


module Annotizer where

import Control.Monad.Extra(concatMapM)
import Control.Monad.State.Lazy
import Data.Bifunctor (first)
import Data.Bifoldable (bifoldl')
import Data.Bitraversable (bitraverse)
import Data.Monoid (Sum(..), Product(..),(<>))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

import L1

localLattice :: L1 -> L2
localLattice = mapExp $ first (\t->(lattice t,t))

mapExp' :: (SourcePos -> ExpF t1 (Exp t2) -> ExpF t2 (Exp t2)) -> Exp t1 -> Exp t2
mapExp' f = foldAnn (\a e -> Ann a $ f a e)

pick :: L2 -> M.Map SourcePos Int -> L1
pick expr src2indx = mapExp' (pick' src2indx) expr
  where
    pick' :: M.Map SourcePos Int -> SourcePos -> ExpF ([Type], Type) (Exp Type) -> ExpF1 L1
    pick' si s (Lam args e (ts,t))             =
      Lam args e $ maybe t (ts !!) $ M.lookup s si
    pick' si s (Bind x (ts,t) e)               =
      Bind x (maybe t (ts !!) $ M.lookup s si) e
    pick' si s (As e (ts,t))                   =
      As e $ maybe t (ts !!) $ M.lookup s si
    pick' si s (DConst x (ts,t) e)             =
      DConst x (maybe t (ts !!) $ M.lookup s si) e
    pick' si s (DLam x xs e (ts,t))            =
      DLam x xs e $ maybe t (ts !!) $ M.lookup s si
    pick' si s (Repeat ii a e1 e2 e b (ts,t))  =
      Repeat ii a e1 e2 e b $ maybe t (ts !!) $ M.lookup s si
    -- Reconstructing terms at the desired type, is there a better way to do this?
    pick' _ _ (Op op es)                       = Op op es
    pick' _ _ (TopLevel es1 es2)               = TopLevel es1 es2
    pick' _ _ (If e1 e2 e3)                    = If e1 e2 e3
    pick' _ _ (App e es)                       = App e es
    pick' _ _ (Ref e)                          = Ref e
    pick' _ _ (DeRef e)                        = DeRef e
    pick' _ _ (Assign e1 e2)                   = Assign e1 e2
    pick' _ _ (GRef e)                         = GRef e
    pick' _ _ (GDeRef e)                       = GDeRef e
    pick' _ _ (GAssign e1 e2)                  = GAssign e1 e2
    pick' _ _ (MRef e)                         = MRef e
    pick' _ _ (MDeRef e)                       = MDeRef e
    pick' _ _ (MAssign e1 e2)                  = MAssign e1 e2
    pick' _ _ (Vect e1 e2)                     = Vect e1 e2
    pick' _ _ (VectRef e1 e2)                  = VectRef e1 e2
    pick' _ _ (VectSet e1 e2 e3)               = VectSet e1 e2 e3
    pick' _ _ (GVect e1 e2)                    = GVect e1 e2
    pick' _ _ (GVectRef e1 e2)                 = GVectRef e1 e2
    pick' _ _ (GVectSet e1 e2 e3)              = GVectSet e1 e2 e3
    pick' _ _ (MVect e1 e2)                    = MVect e1 e2
    pick' _ _ (MVectRef e1 e2)                 = MVectRef e1 e2
    pick' _ _ (MVectSet e1 e2 e3)              = MVectSet e1 e2 e3
    pick' _ _ (Tuple es)                       = Tuple es
    pick' _ _ (TupleProj e i)                  = TupleProj e i
    pick' _ _ (Let es e)                       = Let es e
    pick' _ _ (Letrec es e)                    = Letrec es e
    pick' _ _ (Begin es e)                     = Begin es e
    pick' _ _ (Time e)                         = Time e
    pick' _ _ (P p)                            = P p
    
replaceTypes :: M.Map SourcePos Type -> L1 -> L1
replaceTypes src2pos = foldAnn (\x y -> Ann x (replaceTypes' src2pos x y))
  where
    dyn :: Type -> Type
    dyn (ArrTy l _) = ArrTy (replicate (length l) Dyn) Dyn
    dyn _           = Dyn

    replaceTypes' :: M.Map SourcePos Type -> SourcePos -> ExpF Type L1 -> ExpF Type L1
    replaceTypes' tb s (Lam args e t)      =
      Lam args e $ fromMaybe (dyn t) $ M.lookup s tb
    replaceTypes' tb s (Bind x t e)        =
      Bind x (fromMaybe (dyn t) $ M.lookup s tb) e
    replaceTypes' tb s (As e t)            =
      As e $ fromMaybe (dyn t) $ M.lookup s tb
    replaceTypes' tb s (DConst x t e)      =
      DConst x (fromMaybe (dyn t) $ M.lookup s tb) e
    replaceTypes' tb s (DLam x xs e t)     =
      DLam x xs e $ fromMaybe (dyn t) $ M.lookup s tb
    replaceTypes' tb s (Repeat ii a e1 e2 e b t)  =
      Repeat ii a e1 e2 e b $ fromMaybe (dyn t) $ M.lookup s tb
    replaceTypes' _ _ e                    = e

data TypeInfo = TypeInfo { typePos               :: SourcePos
                         , typeNodesCount        :: Int
                         , typeNodesCountLattice :: [Int]
                         , typeLattice           :: [Type]
                         } deriving Show

-- computes the sizes of local lattices
  -- max # of nodes, src pos, # nodes, ...
genTypeInfo :: L1 -> State Int [TypeInfo]
genTypeInfo (Ann src expr)  = genTypeInfo' src expr
-- it could have defined in terms of a list monoid but that would be
-- inefficient because the associative operation is concatenation. 
-- Furthermore, bifoldl will need to be modified to account for the
-- source position.
-- bifoldl' f const mempty e
--     where
--       f = (\l t ->
--             let ts = lattice t
--             in [TypeInfo undefined (getSum $ static t) (map (getSum . static) ts) ts] <> l)
  where
    static' = getSum . static
    genTypeInfo' s (Lam _ e' t)        =
      get >>= \y -> let st = static' t
                        ts = lattice t
                    in put (st+y) >> (:) <$> return (TypeInfo s st (map static' ts) ts) <*> genTypeInfo e'
    genTypeInfo' s (Bind _ t e) = do
      y <- get
      let st  = static' t
          ts = lattice t
      put (st+y)
      (:) <$> return (TypeInfo s st (map static' ts) ts) <*> genTypeInfo e
    genTypeInfo' s (As e' t)           =
      get >>= \y ->
      let st = static' t
          ts = lattice t
      in put (st+y) >> (:) <$> return (TypeInfo s st (map static' ts) ts) <*> genTypeInfo e'
    genTypeInfo' s (Repeat _ _ e1 e2 e3 b t) = do
      a1 <- genTypeInfo e1
      a2 <- genTypeInfo e2
      y <- get
      let st = static' t
          ts = lattice t
      put (st+y)
      let a3 = [(TypeInfo s st (map static' ts) ts)]
      a4 <- genTypeInfo b
      a5 <- genTypeInfo e3
      return (a1 ++ a2 ++ a3 ++ a4 ++ a5)
    genTypeInfo' s (DConst _ t e) = do
      y <- get
      let st  = static' t
          ts = lattice t
      put (st+y)
      (:) <$> return (TypeInfo s st (map static' ts) ts) <*> genTypeInfo e
    genTypeInfo' s (DLam _ _ e t) = do
      y <- get
      let st  = static' t
          ts = lattice t
      put (st+y)
      (:) <$> return (TypeInfo s st (map static' ts) ts) <*> genTypeInfo e
    genTypeInfo' _ (Op _ es)           = concatMapM genTypeInfo es
    genTypeInfo' _ (If e1 e2 e3)       =
      (\x y z -> x++y++z)
      <$> genTypeInfo e1
      <*> genTypeInfo e2
      <*> genTypeInfo e3
    genTypeInfo' _ (App e1 es)         =
      (++) <$> genTypeInfo e1
      <*> concatMapM genTypeInfo es
    genTypeInfo' _ (TopLevel d e)      =
      (++)
      <$> concatMapM genTypeInfo d
      <*> concatMapM genTypeInfo e
    genTypeInfo' _ (Ref e')            = genTypeInfo e'
    genTypeInfo' _ (DeRef e')          = genTypeInfo e'
    genTypeInfo' _ (Assign e1 e2)      =
      (++)
      <$> genTypeInfo e1
      <*> genTypeInfo e2
    genTypeInfo' _ (GRef e')           = genTypeInfo e'
    genTypeInfo' _ (GDeRef e')         = genTypeInfo e'
    genTypeInfo' _ (GAssign e1 e2)     =
      (++)
      <$> genTypeInfo e1
      <*> genTypeInfo e2
    genTypeInfo' _ (MRef e')           = genTypeInfo e'
    genTypeInfo' _ (MDeRef e')         = genTypeInfo e'
    genTypeInfo' _ (MAssign e1 e2)     =
      (++)
      <$> genTypeInfo e1
      <*> genTypeInfo e2
    genTypeInfo' _ (Vect e1 e2)        =
      (++)
      <$> genTypeInfo e1
      <*> genTypeInfo e2
    genTypeInfo' _ (VectRef e1 e2)     =
      (++)
      <$> genTypeInfo e1
      <*> genTypeInfo e2
    genTypeInfo' _ (VectSet e1 e2 e3)  =
      (\x y z -> x++y++z)
      <$> genTypeInfo e1
      <*> genTypeInfo e2
      <*> genTypeInfo e3
    genTypeInfo' _ (GVect e1 e2)       =
      (++)
      <$> genTypeInfo e1
      <*> genTypeInfo e2
    genTypeInfo' _ (GVectRef e1 e2)    =
      (++)
      <$> genTypeInfo e1
      <*> genTypeInfo e2
    genTypeInfo' _ (GVectSet e1 e2 e3) =
      (\x y z -> x++y++z)
      <$> genTypeInfo e1
      <*> genTypeInfo e2
      <*> genTypeInfo e3
    genTypeInfo' _ (MVect e1 e2)       =
      (++)
      <$> genTypeInfo e1
      <*> genTypeInfo e2
    genTypeInfo' _ (MVectRef e1 e2)    =
      (++)
      <$> genTypeInfo e1
      <*> genTypeInfo e2
    genTypeInfo' _ (MVectSet e1 e2 e3) =
      (\x y z -> x++y++z)
      <$> genTypeInfo e1
      <*> genTypeInfo e2
      <*> genTypeInfo e3
    genTypeInfo' _ (Tuple es)          = concatMapM genTypeInfo es
    genTypeInfo' _ (TupleProj e _)     = genTypeInfo e
    genTypeInfo' _ (Let e1 e2)         =
      (++)
      <$> concatMapM genTypeInfo e1
      <*> genTypeInfo e2
    genTypeInfo' _ (Letrec e1 e2)      =
      (++)
      <$> concatMapM genTypeInfo e1
      <*> genTypeInfo e2
    genTypeInfo' _ (Begin e1 e2)       =
      (++)
      <$> concatMapM genTypeInfo e1
      <*> genTypeInfo e2
    genTypeInfo' _ (Time e')           = genTypeInfo e'
    genTypeInfo' _ _ = return []

class Gradual p where
  -- Generates the lattice of all possible gradually-typed versions.
  lattice :: p -> [p]
  -- Counts the number of less percise programs and the number of
  -- all type constructors
  count   :: p -> (Product Integer,Sum Int)
  -- computes the percentage of dynamic code.
  dynamic :: Int -> p -> Double
  dynamic a e =
    if a > 0
    then fromIntegral (a - getSum (static e)) / fromIntegral a
    else 0
  -- counts the number of static type nodes in a program
  static  :: p -> Sum Int

instance Gradual L1 where
  lattice (Ann i e) = Ann i <$> lattice e
  count (Ann _ e)   = count e
  static (Ann _ e)  = static e

instance (Gradual t,Gradual e) => Gradual (ExpF t e) where
  lattice = bitraverse lattice pure

  count = bifoldl' f g (mempty,mempty)
    where
      f = (\(a,n) x -> (a <> fst (count x),n <> snd (count x)))
      g = (\(a,n) x -> (a <> fst (count x),n <> snd (count x)))

  static = bifoldl' f g mempty
    where
      f = (\n x -> n <> static x)
      g = (\n x -> n <> static x) 

instance Gradual Type where
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

  count (RefTy t)       = let c = count t in (fst c + 1, snd c + 1)
  count (GRefTy t)      = let c = count t in (fst c + 1, snd c + 1)
  count (MRefTy t)      = let c = count t in (fst c + 1, snd c + 1)
  count (VectTy t)      = let c = count t in (fst c + 1, snd c + 1)
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
  count (TupleTy ts)    = let c1 = map count ts
                          in (1 + product (map fst c1),
                              1 + sum (map snd c1))
  count Dyn             = (1,1)
  count _               = (2,1)

  static Dyn            = 0
  static (RefTy t)      = 1 + static t
  static (GRefTy t)     = 1 + static t
  static (MRefTy t)     = 1 + static t
  static (VectTy t)     = 1 + static t
  static (GVectTy t)    = 1 + static t
  static (MVectTy t)    = 1 + static t
  static (FunTy t1 t2)  = 1 + sum (map static (t2:t1))
  static (ArrTy t1 t2)  = sum (map static (t2:t1))
  static (TupleTy ts)   = 1 + sum (map static ts)
  static _              = 1
