module Syntax where

import qualified Data.IntMap as M

data Operator = Inc | Dec | ZeroQ deriving (Show,Eq,Read)

type Name = String

data Exp1 =
  N1 Int
  | B1 Bool
  | Op1 Operator Exp1
  | If1 Exp1 Exp1 Exp1
  | Var1 Name
  | App1 Exp1 Exp1
  | Lam1 Name Exp1
  | Ref1 Exp1
  | DeRef1 Exp1
  | Assign1 Exp1 Exp1
  | Let1 Name Exp1 Exp1
  deriving (Show,Eq,Read)

data Exp2 =
  N2 Int
  | B2 Bool
  | Op2 Operator Exp2
  | If2 Exp2 Exp2 Exp2
  | Var2 Name
  | App2 Exp2 Exp2
  | Lam2 (Name,Type) (Exp2,Type)
  | Ref2 Exp2
  | DeRef2 Exp2
  | Assign2 Exp2 Exp2
  | Let2 (Name,Type,Exp2) Exp2
  deriving (Show,Eq,Read)

type TVName = Int

data Type =
  Dyn
  | IntTy
  | BoolTy
  | FunTy Type Type
  | RefTy Type
  | TVar TVName
  deriving (Show,Eq,Read)

type EnvT = [(Name, Type)]


-- Unification stuff

data Substitution = Substitution Int (M.IntMap Type) deriving Show
