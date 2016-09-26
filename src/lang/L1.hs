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
  | If e e e
  | App e [e]
  | Lam Args e t
  | GRef e
  | GDeRef e
  | GAssign e e
  | MRef e
  | MDeRef e
  | MAssign e e
  | GVect e e -- length value
  | GVectRef e e -- vect pos
  | GVectSet e e e -- vect pos value
  | MVect e e
  | MVectRef e e
  | MVectSet e e e
  | Let (Binds t e) e
  | Letrec (Binds t e) e
  | As e t
  | Begin [e] e
  | Repeat Name Name e e e e t -- int int e
  | Time e
  | P Prim

data Prim =
  Var Name
  | ReadInt
  | N Integer
  | B Bool
  | Unit
  deriving (Eq)

-- newtype Exp = Exp (Fix ExpF)

type ExpF1 = ExpF Type
type L1 = L ExpF1
type ExpF2 = ExpF ([Type],Type)
type L2 = L ExpF2

instance Bifunctor ExpF where
  bimap _ g (Op op es)          = Op op $ map g es
  bimap _ g (If e1 e2 e3)       = If (g e1) (g e2) $ g e3
  bimap _ g (App e1 es)         = App (g e1) $ map g es
  bimap f g (Lam args e t)      = Lam args (g e) $ f t
  bimap _ g (GRef e)            = GRef $ g e
  bimap _ g (GDeRef e)          = GDeRef $ g e
  bimap _ g (GAssign e1 e2)     = GAssign (g e1) $ g e2
  bimap _ g (MRef e)            = MRef $ g e
  bimap _ g (MDeRef e)          = MDeRef $ g e
  bimap _ g (MAssign e1 e2)     = MAssign (g e1) $ g e2
  bimap _ g (GVect e1 e2)       = GVect (g e1) $ g e2
  bimap _ g (GVectRef e1 e2)    = GVectRef (g e1) $ g e2
  bimap _ g (GVectSet e1 e2 e3) = GVectSet (g e1) (g e2) $ g e3
  bimap _ g (MVect e1 e2)       = MVect (g e1) $ g e2
  bimap _ g (MVectRef e1 e2)    = MVectRef (g e1) $ g e2
  bimap _ g (MVectSet e1 e2 e3) = MVectSet (g e1) (g e2) $ g e3
  bimap f g (Let e1 e2)         = Let (bimap f g e1) $ g e2
  bimap f g (Letrec e1 e2)      = Letrec (bimap f g e1) $ g e2
  bimap f g (As e t)            = As (g e) $ f t
  bimap _ g (Begin e' e)        = Begin (map g e') $ g e
  bimap f g (Repeat i a e1 e2 e b t)  = Repeat i a (g e1) (g e2) (g e) (g b) (f t)
  bimap _ g (Time e)            = Time $ g e
  bimap _ _ (P p)               = P p

  
instance Bifunctor Bind where
  bimap f g (Bind x t e) = Bind x (f t) $ g e

instance Bifunctor Binds where
  bimap f g (Binds l) = Binds $ map (bimap f g) l

instance Functor (ExpF t) where
  fmap = bimap id

type Exp t = L (ExpF t)

mapExp :: (ExpF t1 (Exp t2) -> ExpF t2 (Exp t2)) -> Exp t1 -> Exp t2
mapExp f = foldAnn (\p e -> Ann p $ f e)
