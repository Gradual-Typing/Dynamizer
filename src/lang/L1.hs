{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module L1(module Syntax
         ,ExpF(..)
         ,ExpF1
         ,L1
         ,ExpF2
         ,L2
         ,Prim(..)
         ,mapExp
         ,Exp) where

import Data.Bifunctor
import Syntax

-- base functor (two-level types trick)
-- structure operator
data ExpF t e =
  Op Operator [e]
  | DConst Name t e
  | DLam Name Args e t
  | TopLevel [e] [e]
  | If e e e
  | App e [e]
  | Lam Args e t
  | Bind Name t e
  | Ref e
  | DeRef e
  | Assign e e
  | GRef e
  | GDeRef e
  | GAssign e e
  | MRef e
  | MDeRef e
  | MAssign e e
  | Vect e e -- length value
  | VectRef e e -- vect pos
  | VectSet e e e -- vect pos value
  | GVect e e -- length value
  | GVectRef e e -- vect pos
  | GVectSet e e e -- vect pos value
  | MVect e e
  | MVectRef e e
  | MVectSet e e e
  | Tuple [e]
  | TupleProj e Int
  | Let [e] e
  | Letrec [e] e
  | As e t
  | Begin [e] e
  | Repeat Name Name e e e e t
  | Time e
  | P Prim

deriving instance Functor (ExpF t)

deriving instance Foldable (ExpF t)

deriving instance Traversable (ExpF t)

data Prim =
  Var Name
  | N Integer
  | F Double String
  | B Bool
  | Unit
  | C String
  deriving (Eq)

-- newtype Exp = Exp (Fix ExpF)

type ExpF1 = ExpF Type
type L1 = L ExpF1
type ExpF2 = ExpF ([Type],Type)
type L2 = L ExpF2


instance Bifunctor ExpF where
  bimap _ g (Op op es)          = Op op $ map g es
  bimap _ g (TopLevel d e)      = TopLevel (map g d) $ map g e
  bimap _ g (If e1 e2 e3)       = If (g e1) (g e2) $ g e3
  bimap _ g (App e1 es)         = App (g e1) $ map g es
  bimap f g (Lam args e t)      = Lam args (g e) $ f t
  bimap f g (Bind x t e)        = Bind x (f t) $ g e
  bimap _ g (Ref e)             = Ref $ g e
  bimap _ g (DeRef e)           = DeRef $ g e
  bimap _ g (Assign e1 e2)      = Assign (g e1) $ g e2
  bimap _ g (GRef e)            = GRef $ g e
  bimap _ g (GDeRef e)          = GDeRef $ g e
  bimap _ g (GAssign e1 e2)     = GAssign (g e1) $ g e2
  bimap _ g (MRef e)            = MRef $ g e
  bimap _ g (MDeRef e)          = MDeRef $ g e
  bimap _ g (MAssign e1 e2)     = MAssign (g e1) $ g e2
  bimap _ g (Vect e1 e2)        = Vect (g e1) $ g e2
  bimap _ g (VectRef e1 e2)     = VectRef (g e1) $ g e2
  bimap _ g (VectSet e1 e2 e3)  = VectSet (g e1) (g e2) $ g e3
  bimap _ g (GVect e1 e2)       = GVect (g e1) $ g e2
  bimap _ g (GVectRef e1 e2)    = GVectRef (g e1) $ g e2
  bimap _ g (GVectSet e1 e2 e3) = GVectSet (g e1) (g e2) $ g e3
  bimap _ g (MVect e1 e2)       = MVect (g e1) $ g e2
  bimap _ g (MVectRef e1 e2)    = MVectRef (g e1) $ g e2
  bimap _ g (MVectSet e1 e2 e3) = MVectSet (g e1) (g e2) $ g e3
  bimap _ g (Tuple es)          = Tuple $ map g es
  bimap _ g (TupleProj e i)     = TupleProj (g e) i
  bimap _ g (Let e1 e2)         = Let (map g e1) $ g e2
  bimap _ g (Letrec e1 e2)      = Letrec (map g e1) $ g e2
  bimap f g (As e t)            = As (g e) $ f t
  bimap _ g (Begin e' e)        = Begin (map g e') $ g e
  bimap f g (Repeat i a e1 e2 e b t)  = Repeat i a (g e1) (g e2) (g e) (g b) (f t)
  bimap _ g (Time e)            = Time $ g e
  bimap f g (DConst x t e) = DConst x (f t) $ g e
  bimap f g (DLam x args e t) = DLam x args (g e) $ f t
  bimap _ _ (P p)               = P p

-- instance Functor (ExpF t) where
--   fmap = bimap id

type Exp t = L (ExpF t)

mapExp :: (ExpF t1 (Exp t2) -> ExpF t2 (Exp t2)) -> Exp t1 -> Exp t2
mapExp f = foldAnn (\a e -> Ann a $ f e)
