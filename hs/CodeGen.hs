{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module CodeGen (
  schmlCodGen
  ) where

import Text.PrettyPrint

import Syntax

class Pretty p where
  ppe :: p -> Doc

instance Pretty Name where
  ppe = text

instance Pretty L1 where
  ppe (_,e) = ppe e

instance Pretty Exp1 where
  ppe (N1 a) = integer a
  ppe (B1 b) = text (show b)
  ppe Unit = text "()"
  ppe (Op1 op es) = parens $ ppe op <+> hsep (map ppe es)
  ppe (If1 e1 e2 e3) = parens $ text "if" <+> ppe e1 <+> ppe e2 <+> ppe e3
  ppe (Var1 x) = text x
  ppe (App1 e1 es) = parens $ ppe e1 <+> hsep (map ppe es)
  ppe (Lam1 args e t) = parens $ text "lambda" <+> parens (hsep $ map ppe args)
                          <+> (case t of
                                 Dyn -> ppe e
                                 _ -> char ':' <+> ppe t <+> ppe e)
  ppe (GRef1 e) = parens $ text "gbox" <+> ppe e
  ppe (GDeRef1 e) = parens $ text "gunbox" <+> ppe e
  ppe (GAssign1 e1 e2) = parens $ text "gbox-set!" <+> ppe e1 <+> ppe e2
  ppe (MRef1 e) = parens $ text "mbox" <+> ppe e
  ppe (MDeRef1 e) = parens $ text "munbox" <+> ppe e
  ppe (MAssign1 e1 e2) = parens $ text "mbox-set!" <+> ppe e1 <+> ppe e2
  ppe (GVect1 e1 e2) = parens $ text "gvector" <+> ppe e1 <+> ppe e2
  ppe (GVectRef1 e1 e2) = parens $ text "gvector-ref" <+> ppe e1 <+> ppe e2
  ppe (GVectSet1 e1 e2 e3) = parens $ text "gvector-set!" <+> ppe e1 <+> ppe e2 <+> ppe e3
  ppe (MVect1 e1 e2) = parens $ text "mvector" <+> ppe e1 <+> ppe e2
  ppe (MVectRef1 e1 e2) = parens $ text "mvector-ref" <+> ppe e1 <+> ppe e2
  ppe (MVectSet1 e1 e2 e3) = parens $ text "mvector-set!" <+> ppe e1 <+> ppe e2 <+> ppe e3
  ppe (Let1 binds e) = parens $ text "let" <+> parens (hsep $ map ppe binds) <+> ppe e
  ppe (Letrec1 binds e) = parens $ text "letrec" <+> parens (hsep $ map ppe binds) <+> ppe e
  ppe (As1 e t) = parens $ ppe e <+> char ':' <+> ppe t
  ppe (Begin1 es e) = parens $ text "begin" <+> hsep (map ppe es) <+> ppe e
  ppe (Repeat1 x e1 e2 e) = parens $ text "repeat" <+> parens (text x <+> ppe e1 <+> ppe e2) <+> ppe e
  ppe TimerStart1 = text "(timer-start)"
  ppe TimerStop1 = text "(timer-stop)"
  ppe TimerReport1 = text "(timer-report)"

instance Pretty Bind where
  ppe (x,t,e) = brackets (case t of
                            Dyn ->  text x <+> ppe e
                            _ -> text x <+> char ':' <+> ppe t <+> ppe e)

instance Pretty Operator where
  ppe Plus = char '+'
  ppe Minus = char '-'
  ppe Mult = char '*'
  ppe Div = text "%/"
  ppe Eq = char '='
  ppe Ge = text ">="
  ppe Gt = char '>'
  ppe Le = text "<="
  ppe Lt = char '<'
  ppe ShiftR = text "%>>"
  ppe ShiftL = text "%<<"
  ppe BAnd = text "binary-and"
  ppe BOr = text "binary-or"

instance Pretty Arg where
  ppe (x,t) = case t of
    Dyn -> text x
    _ -> brackets $ text x <+> char ':' <+> ppe t

instance Pretty Type where
  ppe Dyn = text "Dyn"
  ppe IntTy = text "Int"
  ppe BoolTy = text "Bool"
  ppe UnitTy = text "()"
  ppe (FunTy t1s t2) = parens $ hsep (map ppe t1s) <> text " -> " <> ppe t2
  ppe (GRefTy t) = parens $ text "GRef" <+> ppe t
  ppe (MRefTy t) = parens $ text "MRef" <+> ppe t
  ppe (GVectTy t) = parens $ text "GVect" <+> ppe t
  ppe (MVectTy t) = parens $ text "MVect" <+> ppe t

schmlCodGen :: L1 -> String
schmlCodGen = render . ppe
