{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Syntax(
  SourcePos,
  Name,
  Args,
  Operator(..),
  Type(..),
  Ann(..),
  L,
  foldAnn) where

import Text.Parsec.Pos (SourcePos)

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

-- newtype Fix e = In {out::e (Fix e)}

-- data Ann a f x = Ann a (f x)
data Ann a e = Ann a (e (Ann a e))

type L a = Ann SourcePos a

-- type AnnT a e = Fix (Compose ((,) a) e)

foldAnn :: Functor e => (a -> e r -> r) -> Ann a e -> r
foldAnn f (Ann a e) = f a (fmap (foldAnn f) e)

-- pattern Ann a e = Fix (Compose (a,e))

-- traverseAnn :: (Applicative f, Functor e1) => (a -> e1 (f (Ann b e2)) -> f (Ann b e2)) -> Ann a e1 -> f (Ann b e2)
-- traverseAnn f (Ann a e) = f a (fmap (traverseAnn f) e)
