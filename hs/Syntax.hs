module Syntax where

import qualified Data.IntMap as M

data Operator = Plus | Minus | Mult | Div | Eq | Ge | Gt | Le | Lt
              | ShiftR | ShiftL | BAnd | BOr
                deriving (Show,Eq,Read)

type Name = String

data Exp1 =
  N1 Int
  | B1 Bool
  | Op1 Operator Exp1 Exp1
  | If1 Exp1 Exp1 Exp1
  | Var1 Name
  | App1 Exp1 Exp1
  | Lam1 (Name,Type) (Exp1,Type)
  | GRef1 Exp1
  | GDeRef1 Exp1
  | GAssign1 Exp1 Exp1
  | Let1 [(Name,Type,Exp1)] Exp1
  | Letrec1 [(Name,Type,Exp1)] Exp1
  | As1 Exp1 Type
  | Begin1 [Exp1] Exp1
  deriving (Show,Eq,Read)

data Type =
  Dyn
  | IntTy
  | BoolTy
  | FunTy Type Type
  | GRefTy Type
  deriving (Show,Eq,Read)

type EnvT = [(Name, Type)]


-- Unification stuff

data Substitution = Substitution Int (M.IntMap Type) deriving Show
