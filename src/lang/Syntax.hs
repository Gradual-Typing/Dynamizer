{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Syntax(
  SourcePos,
  Name,
  Args,
  Operator(..),
  Type(..),
  Ann(..),
  L,
  foldAnn,
  (⊑)) where

import Text.Parsec.Pos (SourcePos)
import Algebra.Lattice

type Name         = String
type Args         = [Name]

data Operator = Plus | Minus | Mult | Div | Eq | Ge | Gt | Le | Lt
              | ShiftR | ShiftL | BAnd | BOr
              | PlusF | MinusF | MultF | DivF| ModuloF | AbsF | LtF
              | LeF | EqF | GtF | GeF | MinF | MaxF | RoundF | FloorF
              | CeilingF | TruncateF | SinF | CosF | TanF | AsinF
              | AcosF | AtanF | LogF | ExpF | SqrtF | ExptF
              | FloatToInt | IntToFloat | CharToInt | ReadInt
              | ReadFloat | ReadChar | DisplayChar
                deriving (Eq,Show)

data Type =
  BlankTy
  | Dyn
  | CharTy
  | IntTy
  | FloatTy
  | BoolTy
  | UnitTy
  | FunTy [Type] Type
  | ArrTy [Type] Type
  | RefTy Type
  | GRefTy Type
  | MRefTy Type
  | VectTy Type
  | GVectTy Type
  | MVectTy Type
  | TupleTy [Type]
  deriving (Eq,Show)

instance MeetSemiLattice Type where
  Dyn /\ t                           = t
  t /\ Dyn                           = t
  CharTy /\ CharTy                   = CharTy
  IntTy /\ IntTy                     = IntTy
  FloatTy /\ FloatTy                 = FloatTy
  BoolTy /\ BoolTy                   = BoolTy
  UnitTy /\ UnitTy                   = UnitTy
  (FunTy ts1 rt1) /\ (FunTy ts2 rt2) =
    FunTy (zipWith (/\) ts1 ts2) $ (/\) rt1 rt2
  (ArrTy ts1 rt1) /\ (ArrTy ts2 rt2) =
    ArrTy (zipWith (/\) ts1 ts2) $ (/\) rt1 rt2
  (RefTy t1) /\ (RefTy t2)           = RefTy $ (/\) t1 t2
  (GRefTy t1) /\ (GRefTy t2)         = GRefTy $ (/\) t1 t2
  (MRefTy t1) /\ (MRefTy t2)         = MRefTy $ (/\) t1 t2
  (VectTy t1) /\ (VectTy t2)         = VectTy $ (/\) t1 t2
  (GVectTy t1) /\ (GVectTy t2)       = GVectTy $ (/\) t1 t2
  (MVectTy t1) /\ (MVectTy t2)       = MVectTy $ (/\) t1 t2
  (TupleTy t1) /\ (TupleTy t2)       = TupleTy $ zipWith (/\) t1 t2
  t1 /\ t2                             =
    error ("/\\: undefined on " ++ show t1 ++ " and " ++ show t2)

instance JoinSemiLattice Type where
  Dyn \/ _                           = Dyn
  _ \/ Dyn                           = Dyn
  CharTy \/ CharTy                   = CharTy
  IntTy \/ IntTy                     = IntTy
  FloatTy \/ FloatTy                 = FloatTy
  BoolTy \/ BoolTy                   = BoolTy
  UnitTy \/ UnitTy                   = UnitTy
  (FunTy ts1 rt1) \/ (FunTy ts2 rt2) =
    FunTy (zipWith (\/) ts1 ts2) $ (\/) rt1 rt2
  (ArrTy ts1 rt1) \/ (ArrTy ts2 rt2) =
    ArrTy (zipWith (\/) ts1 ts2) $ (\/) rt1 rt2
  (RefTy t1) \/ (RefTy t2)           = RefTy $ (\/) t1 t2
  (GRefTy t1) \/ (GRefTy t2)         = GRefTy $ (\/) t1 t2
  (MRefTy t1) \/ (MRefTy t2)         = MRefTy $ (\/) t1 t2
  (VectTy t1) \/ (VectTy t2)         = VectTy $ (\/) t1 t2
  (GVectTy t1) \/ (GVectTy t2)       = GVectTy $ (\/) t1 t2
  (MVectTy t1) \/ (MVectTy t2)       = MVectTy $ (\/) t1 t2
  (TupleTy t1) \/ (TupleTy t2)       = TupleTy $ zipWith (\/) t1 t2
  t1 \/ t2                           =
    error ("\\/: undefined on " ++ show t1 ++ " and " ++ show t2)

instance Lattice Type where

(⊑) :: Type -> Type -> Bool
(⊑) = joinLeq

data Ann a e = Ann a (e (Ann a e))

type L a = Ann SourcePos a

-- type AnnT a e = Fix (Compose ((,) a) e)

foldAnn :: Functor e => (a -> e r -> r) -> Ann a e -> r
foldAnn f (Ann a e) = f a (fmap (foldAnn f) e)

-- pattern Ann a e = Fix (Compose (a,e))

-- traverseAnn :: (Applicative f, Functor e1) => (a -> e1 (f (Ann b e2)) -> f (Ann b e2)) -> Ann a e1 -> f (Ann b e2)
-- traverseAnn f (Ann a e) = f a (fmap (traverseAnn f) e)
