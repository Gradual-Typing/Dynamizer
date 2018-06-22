{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Dynamizer.Lattice where

import           Control.Arrow         ((***))
import           Control.Monad.CSP     (allCSPSolutions, constraint, mkDV)
import           Data.Bifoldable       (Bifoldable, bifoldMap)
import           Data.Bifunctor        (bimap)
import           Data.Bitraversable    (bitraverse)
import qualified Data.DList            as DL
import           Data.List             (transpose)
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromMaybe)
import           Data.Monoid           (Product (..), Sum (..))

import           Language.Grift.Source.Syntax


embedLocalLattice :: forall a t. Gradual (t (Ann a t))
                  => Ann a (ExpF (Ann a t))
                  -> Ann a (ExpF (DL.DList (Ann a t), Ann a t))
embedLocalLattice (Ann s e) = Ann s $ bimap (\t -> (lattice t, t)) embedLocalLattice e

pick :: forall a t. (Gradual (t (Ann a t)), Ord a)
  => Ann a (ExpF (DL.DList (Ann a t), Ann a t))
  -> M.Map a Int
  -> Ann a (ExpF (Ann a t))
pick (Ann s e) src2indx = Ann s $ bimap pick' (`pick` src2indx) e
  where
    pick' :: (DL.DList (Ann a t), Ann a t) -> Ann a t
    pick' (DL.toList -> ts, t@(Ann s' _)) = maybe t (ts !!) $ M.lookup s' src2indx

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

class Dynamize p where
  dynamize :: p -> p

instance Dynamize (e (Ann a e)) => Dynamize (Ann a e) where
  dynamize (Ann a e) = Ann a $ dynamize e

instance (Dynamize t, Dynamize e) => Dynamize (ExpF t e) where
  dynamize = bimap dynamize dynamize

instance Dynamize (Type (Ann a Type)) where
  dynamize (ArrTy ts (Ann a _)) = ArrTy (map (\(Ann a' _) -> Ann a' Dyn) ts) $ Ann a Dyn
  dynamize _ = Dyn

class Gradual p where
  -- Generates the lattice of all possible gradually-typed versions.
  lattice :: p -> DL.DList p
  -- Generates the lattice of all coarce grained gradual typing on the function
  -- level
  funLattice :: p -> DL.DList p
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
  lattice (Ann i e)    = Ann i <$> lattice e
  funLattice (Ann a e) = Ann a <$> funLattice e
  count   (Ann _ e)    = count e
  static  (Ann _ e)    = static e

instance (Gradual t, Gradual e, Dynamize t, Dynamize e) => Gradual (ExpF t e) where
  lattice = bitraverse lattice pure
  count   = bifoldMap count count
  static  = bifoldMap static static

  funLattice e@(DLam name args e' t) = [e, DLam name args (dynamize e') $ dynamize t]
  funLattice e@(Lam args body t) = [e, Lam args (dynamize body) $ dynamize t]
  funLattice e = bitraverse pure funLattice e

instance Gradual t => Gradual (Type t) where
  lattice (RefTy t)     = DL.cons Dyn (RefTy <$> lattice t)
  lattice (GRefTy t)    = DL.cons Dyn (GRefTy <$> lattice t)
  lattice (MRefTy t)    = DL.cons Dyn (MRefTy <$> lattice t)
  lattice (VectTy t)    = DL.cons Dyn (VectTy <$> lattice t)
  lattice (GVectTy t)   = DL.cons Dyn (GVectTy <$> lattice t)
  lattice (MVectTy t)   = DL.cons Dyn (MVectTy <$> lattice t)
  lattice (FunTy t1 t2) = DL.cons Dyn (FunTy <$> mapM lattice t1 <*> lattice t2)
  lattice (ArrTy t1 t2) = ArrTy <$> mapM lattice t1 <*> lattice t2
  lattice (TupleTy ts)  = DL.cons Dyn (TupleTy <$> mapM lattice ts)
  lattice CharTy        = DL.fromList [Dyn, CharTy]
  lattice IntTy         = DL.fromList [Dyn, IntTy]
  lattice FloatTy       = DL.fromList [Dyn, FloatTy]
  lattice BoolTy        = DL.fromList [Dyn, BoolTy]
  lattice UnitTy        = DL.fromList [Dyn, UnitTy]
  lattice Dyn           = DL.singleton Dyn
  lattice BlankTy       = DL.singleton BlankTy

  funLattice _ = error "funLattice is undefined over arbitrary types"

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
  count BlankTy       = (1, 1)
  count CharTy        = (2, 1)
  count IntTy         = (2, 1)
  count FloatTy       = (2, 1)
  count BoolTy        = (2, 1)
  count UnitTy        = (2, 1)

  static Dyn           = 0
  static BlankTy       = 0
  static CharTy        = 1
  static IntTy         = 1
  static FloatTy       = 1
  static BoolTy        = 1
  static UnitTy        = 1
  static (RefTy t)     = 1 + static t
  static (GRefTy t)    = 1 + static t
  static (MRefTy t)    = 1 + static t
  static (VectTy t)    = 1 + static t
  static (GVectTy t)   = 1 + static t
  static (MVectTy t)   = 1 + static t
  static (FunTy t1 t2) = 1 + sum (map static (t2:t1))
  static (ArrTy t1 t2) = sum (map static (t2:t1))
  static (TupleTy ts)  = 1 + sum (map static ts)

genLatticeInfo :: forall e a. Bifoldable e
               => Ann a (e (Ann a Type))
               -> ([Ann (Int, a) Type], Int)
genLatticeInfo = (DL.toList *** getSum) . localLattice
  where
    localLattice :: Ann a (e (Ann a Type)) -> (DL.DList (Ann (Int, a) Type), Sum Int)
    localLattice (Ann _ e) = bifoldMap f localLattice e

    f :: Ann a Type -> (DL.DList (Ann (Int, a) Type), Sum Int)
    f t = (DL.singleton ct, Sum n)
      where ct@(Ann (n, _) _) = addCount t

getCount :: forall a. Ann (Int, a) Type -> Int
getCount (Ann (x,_) _) = x

getAnn :: forall a b. Ann (b, a) Type -> a
getAnn (Ann (_,a) _) = a

addCount :: forall a. Ann a Type -> Ann (Int, a) Type
addCount = bottomUp f
  where
    bottomUp :: (a -> Type (Ann (Int, a) Type) -> Ann (Int, a) Type)
             -> Ann a Type
             -> Ann (Int, a) Type
    bottomUp fn (Ann a t) = fn a $ bottomUp fn <$> t

    f :: a -> Type (Ann (Int, a) Type) -> Ann (Int, a) Type
    f a BlankTy = Ann (0, a) BlankTy
    f a Dyn     = Ann (0, a) Dyn
    f a CharTy  = Ann (1, a) CharTy
    f a IntTy   = Ann (1, a) IntTy
    f a FloatTy = Ann (1, a) FloatTy
    f a BoolTy  = Ann (1, a) BoolTy
    f a UnitTy  = Ann (1, a) UnitTy
    f a t@(RefTy (Ann (n, _) _))   = Ann (n+1, a) t
    f a t@(GRefTy (Ann (n, _) _))  = Ann (n+1, a) t
    f a t@(MRefTy (Ann (n, _) _))  = Ann (n+1, a) t
    f a t@(VectTy (Ann (n, _) _))  = Ann (n+1, a) t
    f a t@(GVectTy (Ann (n, _) _)) = Ann (n+1, a) t
    f a t@(MVectTy (Ann (n, _) _)) = Ann (n+1, a) t
    f a t@(FunTy ts rt) = Ann ((+) 1 $ sum $ map getCount (rt:ts), a) t
    f a t@(ArrTy ts rt) = Ann (sum $ map getCount (rt:ts), a) t
    f a t@(TupleTy ts)  = Ann ((+) 1 $ sum $ map getCount ts, a) t

stripCount :: Ann (Int, a) Type -> Ann a Type
stripCount (Ann (_, a) t) = Ann a $ stripCount <$> t

genLessPreciseType :: forall a. Int -> Ann (Int, a) Type -> [Ann a Type]
genLessPreciseType nodes ty'@(Ann (n'', _) _) | nodes < 0 || nodes > n'' = []
                                              | otherwise = f ty' nodes
  where
    -- generate $ns' such that (sum ns') = $n-1, each number in ns' <= the
    -- corresponding number in $ns, and there is y, y' \in $ns' that are
    -- different in each combination.
    gen :: Int -> [Int] -> [[Int]]
    gen n ns = allCSPSolutions $ do
      dvs <- mapM (\a -> mkDV [0 .. a]) ns
      constraint ((== n) . sum) dvs
      return dvs

    f :: Ann (Int, a) Type -> Int -> [Ann a Type]
    f (Ann _ Dyn)               _ = error "genLessPreciseType: unexpected Dyn"
    f (Ann _ (ArrTy _ _))       0 = []
    f t@(Ann (n', a) _)         n | n == n' = [stripCount t] -- otherwise n < n'
                                  | n == 0  = [Ann a Dyn]
    f (Ann (_, a) BlankTy)      _ = [Ann a BlankTy]
    f (Ann (_, a) CharTy)       _ = [Ann a CharTy]
    f (Ann (_, a) IntTy)        _ = [Ann a IntTy]
    f (Ann (_, a) FloatTy)      _ = [Ann a FloatTy]
    f (Ann (_, a) BoolTy)       _ = [Ann a BoolTy]
    f (Ann (_, a) UnitTy)       _ = [Ann a UnitTy]
    f (Ann (_, a) (RefTy t))    n = (Ann a . RefTy) <$> f t (n-1)
    f (Ann (_, a) (GRefTy t))   n = (Ann a . GRefTy) <$> f t (n-1)
    f (Ann (_, a) (MRefTy t))   n = (Ann a . MRefTy) <$> f t (n-1)
    f (Ann (_, a) (VectTy t))   n = (Ann a . VectTy) <$> f t (n-1)
    f (Ann (_, a) (GVectTy t))  n = (Ann a . GVectTy) <$> f t (n-1)
    f (Ann (_, a) (MVectTy t))  n = (Ann a . MVectTy) <$> f t (n-1)
    f (Ann (_, a) (FunTy ts t)) n = g a (n-1) FunTy (t:ts)
    f (Ann (_, a) (ArrTy ts t)) n = g a n ArrTy (t:ts)
    f (Ann (_, a) (TupleTy ts)) n =
      map (Ann a) $ concatMap (map TupleTy . transpose . zipWith f ts) $ gen (n-1) $ map getCount ts

    g :: a
      -> Int
      -> ([Ann a Type] -> Ann a Type -> Type (Ann a Type))
      -> [Ann (Int, a) Type]
      -> [Ann a Type]
    g a n c ts =
      let temp :: [[[Ann a Type]]] = map (zipWith f ts) $ gen n $ map getCount ts
          rts  :: [[Ann a Type]]   = map head temp
          args :: [[[Ann a Type]]] = map tail temp
      in map (Ann a) $ concat $ zipWith (zipWith c) (map transpose args) rts
