{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Syntax where

import Text.Parsec.Pos (SourcePos)
import Data.Functor.Foldable
import Data.Functor.Compose

type Name = String
type Args = [Name]
data Bind t e = Bind Name t e
newtype Binds t e = Binds [Bind t e]

data Operator = Plus | Minus | Mult | Div | Eq | Ge | Gt | Le | Lt
              | ShiftR | ShiftL | BAnd | BOr
                deriving (Eq,Show)

data Type =
  Dyn
  | IntTy
  | BoolTy
  | UnitTy
  | FunTy [Type] Type
  | ArrTy [Type] Type
  | GRefTy Type
  | MRefTy Type
  | GVectTy Type
  | MVectTy Type
  deriving (Eq,Show)

-- newtype Fix e = In {out::e (Fix e)}

-- data Ann a f x = Ann a (f x)
data Ann a e = Ann a (e (Ann a e))

type L a = Ann SourcePos a

type AnnT a e = Fix (Compose ((,) a) e)

foldAnn :: Functor e => (a -> e r -> r) -> Ann a e -> r
foldAnn f (Ann a e) = f a (fmap (foldAnn f) e)

-- pattern Ann a e = Fix (Compose (a,e))
