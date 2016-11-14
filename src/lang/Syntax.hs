{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Syntax where

import Text.Parsec.Pos (SourcePos)
import Data.Functor.Foldable
import Data.Functor.Compose

type Name         = String
type Args         = [Name]
data Bind t e     = Bind Name t e
newtype Binds t e = Binds [Bind t e]
data Def t e      = DConst Name t e | DLam Name Args e t
data Defs t e     = Defs [Def t e]

data Operator = Plus | Minus | Mult | Div | Eq | Ge | Gt | Le | Lt
              | ShiftR | ShiftL | BAnd | BOr
              | PlusF | MinusF | MultF | DivF| ModuloF | AbsF | LtF
              | LeF | EqF | GtF | GeF | MinF | MaxF | RoundF | FloorF
              | CeilingF | TruncateF | SinF | CosF | TanF | AsinF
              | AcosF | AtanF | LogF | ExpF | SqrtF | ExptF
              | FloatToInt | IntToFloat
                deriving (Eq,Show)

data Type =
  BlankTy
  | Dyn
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
  deriving (Eq,Show)

-- newtype Fix e = In {out::e (Fix e)}

-- data Ann a f x = Ann a (f x)
data Ann a e = Ann a (e (Ann a e))

type L a = Ann SourcePos a

type AnnT a e = Fix (Compose ((,) a) e)

foldAnn :: Functor e => (a -> e r -> r) -> Ann a e -> r
foldAnn f (Ann a e) = f a (fmap (foldAnn f) e)

-- pattern Ann a e = Fix (Compose (a,e))
