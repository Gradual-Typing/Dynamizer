{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Dynamizer.Lattice where

import           Control.Arrow         ((***), (&&&))
import           Data.Bifoldable       (Bifoldable, bifoldMap)
import           Data.Bifunctor        (bimap)
import           Data.Bitraversable    (bitraverse)
import qualified Data.DList            as DL
import           Data.Foldable         (fold)
import           Data.List             (replicate)
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromMaybe, fromJust)
import           Data.Monoid           (Product (..), Sum (..))

import           Language.Grift.Source.Syntax
import           Language.Grift.Source.Utils

import           Dynamizer.Module


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
    replaceTypes' :: Ann a Type -> Ann a Type
    replaceTypes' t@(Ann s' _) = fromMaybe (dynamize t) $ M.lookup s' src2ty

class Dynamizable p where
  dynamize :: p -> p

instance Dynamizable (e (Ann a e)) => Dynamizable (Ann a e) where
  dynamize (Ann a e) = Ann a $ dynamize e

instance (Dynamizable t, Dynamizable e) => Dynamizable (ExpF t e) where
  dynamize = bimap dynamize dynamize

instance Dynamizable t => Dynamizable (Type t) where
  dynamize t@ArrTy{} = dynamize <$> t
  dynamize _         = Dyn

class Dynamizable p => Gradual p where
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
  -- counts the number of functions.
  funCount :: p -> Sum Int

-- TODO: think about defining a generalized notion of Functor, Foldable, and
-- Traversable for Ann and use it here
instance Gradual (e (Ann a e)) => Gradual (Ann a e) where
  lattice    (Ann i e) = Ann i <$> lattice e
  funLattice (Ann a e) = Ann a <$> funLattice e
  count      (Ann _ e) = count e
  static     (Ann _ e) = static e
  funCount   (Ann _ e) = funCount e

instance (Gradual t, Gradual e) => Gradual (ExpF t e) where
  lattice = bitraverse lattice lattice
  count   = bifoldMap count count
  static  = bifoldMap static static

  funLattice e@DLam{} = [e, bimap dynamize dynamize e]
  funLattice e@Lam{}  = [e, bimap dynamize dynamize e]
  funLattice e        = traverse funLattice e

  funCount DLam{}   = 1
  funCount Lam{}    = 1
  funCount e        = foldMap funCount e

instance Gradual t => Gradual (Type t) where
  lattice Dyn       = pure Dyn
  lattice BlankTy   = pure BlankTy
  lattice t@ArrTy{} = traverse lattice t
  lattice t         = DL.cons Dyn $ traverse lattice t

  count Dyn       = (1, 1)
  count BlankTy   = (1, 1)
  count t@ArrTy{} = foldMap count t
  count t         = ((+1) *** (+1)) $ foldMap count t

  static Dyn       = 0
  static BlankTy   = 0
  static t@ArrTy{} = foldMap static t
  static t         = 1 + foldMap static t

  funLattice = pure

  funCount = mempty

annotateTypeWithCount :: forall a. Ann a Type -> Ann (a, Sum Int) Type
annotateTypeWithCount = bottomUp (\a e -> (a, f e))
  where
    f :: Type (Ann (a, Sum Int) Type) -> Sum Int
    f BlankTy   = 0
    f Dyn       = 0
    f e@ArrTy{} = foldMap getSnd e
    f e         = 1 + foldMap getSnd e

genLatticeInfo :: forall e a. Bifoldable e
               => Ann a (e (Ann a Type))
               -> ([Ann (a, Sum Int) Type], Int)
genLatticeInfo = (DL.toList *** getSum) . localLattice
  where
    localLattice :: Ann a (e (Ann a Type)) -> (DL.DList (Ann (a, Sum Int) Type), Sum Int)
    localLattice (Ann _ e) = bifoldMap ((pure &&& getSnd) . annotateTypeWithCount) localLattice e

coarseLattice :: forall a t. Gradual t => Int -- number of units to gradualize over
              -> Ann a (ExpF t)               -- the input program
              -> [Ann a (ExpF t)]             -- the list of partially typed programs
coarseLattice unitCount p = f $ funCount p
  where
    f n | n < 10    = DL.toList $ funLattice p
        | otherwise = bigCoarseLattice unitCount p

bigCoarseLattice :: forall a t. Gradual t 
                 => Int              -- the desired number of modules to
                                     -- gradualize over. The actual number of
                                     -- modules will be less than or equal to
                                     -- that number.
                 -> Ann a (ExpF t)   -- the input program
                 -> [Ann a (ExpF t)]
bigCoarseLattice unitCount p = map (\(bit, m) -> mapAnn m bit Nothing p) maps
  where
    modules = computeModules (unitCount - 1) p

    mapAnn :: M.Map FunName Bool -- maps function names to boolean that indicate
                                 -- whether to dynamize that function as part of
                                 -- the current module.
           -> Bool               -- determines if the region that does not
                                 -- belong to any function should be dynamized.
           -> Maybe FunName      -- the name of the current bound function.
           -> Ann a (ExpF t)     -- the input program
           -> Ann a (ExpF t)
    mapAnn info bit name (Ann a e) = f info bit name a e

    f :: M.Map FunName Bool
      -> Bool
      -> Maybe FunName
      -> a
      -> ExpF t (Ann a (ExpF t))
      -> Ann a (ExpF t)
    f info bit _ a e@(DLam name _ _ _) =
      Ann a $ case M.lookup name info of
                Just True -> dynamize e
                _         -> mapAnn info bit (Just name) <$> e
    f info bit name a e@Lam{} =
      Ann a $ case M.lookup (fromJust name) info of
                Just True -> dynamize e
                _         -> mapAnn info bit name <$> e
    f info bit _ a (Bind name t b) =
      Ann a $ Bind name (if bit then dynamize t else t) $ mapAnn info bit (Just name) b
    f info bit name a e@DConst{} =
      Ann a $ bimap (\t -> if bit then dynamize t else t) (mapAnn info bit name) e
    f info bit name a e = Ann a $ mapAnn info bit name <$> e

    configs = sequence @[] @[] @Bool $ replicate unitCount [True, False]

    maps = map (head &&& (M.fromList . fold . zipWith (\ m b -> map (, b) m) modules . tail)) configs
