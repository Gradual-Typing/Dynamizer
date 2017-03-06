{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE TemplateHaskell #-}
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

import Data.Bifunctor.TH
import Syntax

-- base functor (two-level types trick)
-- structure operator
data ExpF t e =
  DConst Name t e
  | DLam Name Args e t
  | Lam Args e t
  | Bind Name t e
  | As e t
  | Repeat Name Name e e e e t
  | Op Operator [e]
  | TopLevel [e] [e]
  | If e e e
  | App e [e]
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
  | Begin [e] e
  | Time e
  | P Prim

data Prim =
  Var Name
  | N Integer
  | F Double String
  | B Bool
  | Unit
  | C String
  deriving (Eq)

deriving instance Functor (ExpF t)
deriving instance Foldable (ExpF t)
deriving instance Traversable (ExpF t)

$(deriveBifunctor ''ExpF)
$(deriveBifoldable ''ExpF)
$(deriveBitraversable ''ExpF)

-- newtype Exp = Exp (Fix ExpF)

type ExpF1 = ExpF Type
type L1 = L ExpF1
type ExpF2 = ExpF ([Type],Type)
type L2 = L ExpF2

type Exp t = L (ExpF t)

mapExp :: (ExpF t1 (Exp t2) -> ExpF t2 (Exp t2)) -> Exp t1 -> Exp t2
mapExp f = foldAnn (\a e -> Ann a $ f e)
