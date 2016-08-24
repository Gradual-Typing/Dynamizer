{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module CodeGen (
  codeGen,
  Pretty(..)
  ) where

import Text.PrettyPrint

import L1

indent :: Doc -> Doc
indent = nest 2

vcat' :: [Doc] -> Doc
vcat' = foldr ($+$) empty

class Pretty p where
  ppe :: p -> Doc

instance Pretty Name where
  ppe = text

instance Pretty L1 where
  ppe (Ann _ e) = ppe e

instance Pretty Prim where
  ppe (Var x)                = text x
  ppe ReadInt                = text "(read-int)"
  ppe (N a)                  = integer a
  ppe (B b)                  = ppe b
  ppe Unit                   = text "()"

instance Pretty e => Pretty (ExpF1 e) where
  ppe (Op op es)             = parens $ ppe op <+> hsep (map ppe es)
  ppe (If e1 e2 e3)          = parens $ text "if" <+> ppe e1 $+$ indent (ppe e2) $+$ indent (ppe e3)
  ppe (App e1 es)            = parens $ ppe e1 <+> hsep (map ppe es)
  ppe (Lam s e (ArrTy ts t)) = parens $ (text "lambda" <+> parens
                                         (vcat' (zipWith (\a -> \case
                                                            --Dyn -> a
                                                            t' -> lbrack <> a
                                                                  <+> char ':'
                                                                  <+> ppe t' <> rbrack)
                                                (map ppe s) ts)) <+>
                                         char ':' <+> ppe t) $+$ indent (ppe e)
                                         -- if t == Dyn then empty
                                         -- else
  -- ppe (Lam s e _) = parens $ (text "lambda" <+> parens
  --                                        (hsep (map ppe s))) $+$ (indent $ ppe e)
  ppe (Lam _ _ t)            = error ("lambda with type other than arrow" ++ show t)
  ppe (GRef e)               = parens $ text "gbox" <+> ppe e
  ppe (GDeRef e)             = parens $ text "gunbox" <+> ppe e
  ppe (GAssign e1 e2)        = parens $ text "gbox-set!" <+> ppe e1 <+> ppe e2
  ppe (MRef e)               = parens $ text "mbox" <+> ppe e
  ppe (MDeRef e)             = parens $ text "munbox" <+> ppe e
  ppe (MAssign e1 e2)        = parens $ text "mbox-set!" <+> ppe e1 <+> ppe e2
  ppe (GVect e1 e2)          = parens $ text "gvector" <+> ppe e1 <+> ppe e2
  ppe (GVectRef e1 e2)       = parens $ text "gvector-ref" <+> ppe e1 <+> ppe e2
  ppe (GVectSet e1 e2 e3)    = parens $ text "gvector-set!" <+> ppe e1 <+> ppe e2
                               <+> ppe e3
  ppe (MVect e1 e2)          = parens $ text "mvector" <+> ppe e1 <+> ppe e2
  ppe (MVectRef e1 e2)       = parens $ text "mvector-ref" <+> ppe e1 <+> ppe e2
  ppe (MVectSet e1 e2 e3)    = parens $ text "mvector-set!" <+> ppe e1 <+> ppe e2
                               <+> ppe e3
  ppe (Let binds e)          = parens $ text "let" <+> parens (vcat' $ map ppe binds)
                               $+$ indent (ppe e)
  ppe (Letrec binds e)       = parens $ text "letrec" <+> parens (vcat' $ map ppe binds)
                               $+$ indent (ppe e)
  ppe (As e t)               = parens $ ppe e <+> char ':' <+> ppe t
  ppe (Begin es e)           = parens $ text "begin" $+$ indent (vcat' $ map ppe es) $+$ indent (ppe e)
  ppe (Repeat x e1 e2 e a b c)     =
    parens $ text "repeat" <+> parens (text x <+> ppe e1 <+> ppe e2) <+> parens (text a <+>
                                                                                 (case c of
                                                                                   Just t -> char ':' <+> ppe t
                                                                                   _ -> empty) <+> ppe b) $+$ ppe e
  ppe (Time e)               = parens $ text "time" <+> ppe e
  ppe (P p)                  = ppe p

instance (Pretty e,Pretty t) => Pretty (Bind e t) where
  ppe (x,t,e) =
    brackets (text x <+> char ':' <+> ppe t $+$ indent (ppe e))

instance Pretty Operator where
  ppe Plus   = char '+'
  ppe Minus  = char '-'
  ppe Mult   = char '*'
  ppe Div    = text "%/"
  ppe Eq     = char '='
  ppe Ge     = text ">="
  ppe Gt     = char '>'
  ppe Le     = text "<="
  ppe Lt     = char '<'
  ppe ShiftR = text "%>>"
  ppe ShiftL = text "%<<"
  ppe BAnd   = text "binary-and"
  ppe BOr    = text "binary-or"

instance Pretty Bool where
  ppe True = text "#t"
  ppe False = text "#f"

instance Pretty Type where
  ppe Dyn          = text "Dyn"
  ppe IntTy        = text "Int"
  ppe BoolTy       = text "Bool"
  ppe UnitTy       = text "()"
  ppe (FunTy ts t) = parens $ hsep (map ppe ts) <> text " -> " <> ppe t
  ppe (ArrTy _ _)  = error "arrow type should not be prettied"
  ppe (GRefTy t)   = parens $ text "GRef" <+> ppe t
  ppe (MRefTy t)   = parens $ text "MRef" <+> ppe t
  ppe (GVectTy t)  = parens $ text "GVect" <+> ppe t
  ppe (MVectTy t)  = parens $ text "MVect" <+> ppe t

codeGen :: Pretty p => p -> String
codeGen = render . ppe
