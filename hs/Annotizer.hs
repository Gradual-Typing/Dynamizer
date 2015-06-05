{-# LANGUAGE LambdaCase, TupleSections #-}

module Annotizer where

import System.Directory (createDirectoryIfMissing)
import Control.Monad (liftM, liftM2, liftM3)

import Syntax

nAnnotize' :: Exp1 -> [Exp1]
nAnnotize' (Op1 op e1 e2) = liftM2 (Op1 op) (nAnnotize' e1) $ nAnnotize' e2
nAnnotize' (If1 e1 e2 e3) = liftM3 If1 (nAnnotize' e1) (nAnnotize' e2) $ nAnnotize' e3
nAnnotize' (App1 e1 e2) = liftM2 App1 (nAnnotize' e1) $ nAnnotize' e2
nAnnotize' (Lam1 (x,t1) (e,t2)) =
  nAnnotizeTy' t1 >>= \t1' -> nAnnotizeTy' t2 >>= \t2' -> nAnnotize' e >>= \e' -> return $ Lam1 (x,t1') (e',t2')
nAnnotize' (GRef1 e) = liftM GRef1 $ nAnnotize' e
nAnnotize' (GDeRef1 e) = liftM GDeRef1 $ nAnnotize' e
nAnnotize' (GAssign1 e1 e2) = liftM2 GAssign1 (nAnnotize' e1) $ nAnnotize' e2
nAnnotize' (Let1 e1 e2) = mapM nAnnotizeCls' e1 >>= \e1' -> liftM (Let1 e1') $ nAnnotize' e2
nAnnotize' (Letrec1 e1 e2) = mapM nAnnotizeCls' e1 >>= \e1' -> liftM (Letrec1 e1') $ nAnnotize' e2
nAnnotize' (As1 e t) = liftM2 As1 (nAnnotize' e) $ nAnnotizeTy' t
nAnnotize' e = [e]

nAnnotizeCls' :: (Name,Type,Exp1) -> [(Name,Type,Exp1)]
nAnnotizeCls' (x,t,e) = nAnnotizeTy' t >>= \t' -> liftM (x,t',) $ nAnnotize' e

nAnnotizeTy' :: Type -> [Type]
nAnnotizeTy' (GRefTy t) = Dyn: map GRefTy (nAnnotizeTy' t)
nAnnotizeTy' (FunTy t1 t2) = Dyn:liftM2 FunTy (nAnnotizeTy' t1) (nAnnotizeTy' t2)
nAnnotizeTy' t = [t,Dyn]

schmlCodGen :: Exp1 -> String -> String
schmlCodGen (N1 n) =  shows n
schmlCodGen (B1 b) | b = ("#t"++) | not b = ("#f"++)
schmlCodGen (Op1 op e1 e2) =
  case op of
   Plus -> ("(+ " ++) . schmlCodGen e1 . (' ':) . schmlCodGen e2 . (')':)
   Minus -> ("(- " ++) . schmlCodGen e1 . (' ':) . schmlCodGen e2 . (')':)
   Mult -> ("(* " ++) . schmlCodGen e1 . (' ':) . schmlCodGen e2 . (')':)
   Div -> ("(%/ " ++) . schmlCodGen e1 . (' ':) . schmlCodGen e2 . (')':)
   Eq -> ("(= " ++) . schmlCodGen e1 . (' ':) . schmlCodGen e2 . (')':)
   Ge -> ("(>= " ++) . schmlCodGen e1 . (' ':) . schmlCodGen e2 . (')':)
   Le -> ("(<= " ++) . schmlCodGen e1 . (' ':) . schmlCodGen e2 . (')':)
   Gt -> ("(> " ++) . schmlCodGen e1 . (' ':) . schmlCodGen e2 . (')':)
   Lt -> ("(< " ++) . schmlCodGen e1 . (' ':) . schmlCodGen e2 . (')':)
   ShiftR -> ("(%>> " ++) . schmlCodGen e1 . (' ':) . schmlCodGen e2 . (')':)
   ShiftL -> ("(%<< " ++) . schmlCodGen e1 . (' ':) . schmlCodGen e2 . (')':)
   BAnd -> ("(binary-and " ++) . schmlCodGen e1 . (' ':) . schmlCodGen e2 . (')':)
   BOr -> ("(binary-or " ++) . schmlCodGen e1 . (' ':) . schmlCodGen e2 . (')':)
schmlCodGen (If1 e1 e2 e3) = ("(if " ++) . schmlCodGen e1 . ("\n"++) . schmlCodGen e2 . ("\n"++) . schmlCodGen e3 . (')':)
schmlCodGen (Var1 x) = (x++)
schmlCodGen (App1 e1 e2) = ('(':) . schmlCodGen e1 . (' ':) . schmlCodGen e2 . (')':)
schmlCodGen (Lam1 (x,t1) (e,t2)) = ("(lambda ("++) . 
                                   (case t1 of
                                     Dyn -> (x++)
                                     _ -> ('[':) . (x++) . (" : "++) . (schmlTyGen t1 ++) . (']':))
                                   . (") : "++) . (schmlTyGen t2 ++) . ("\n"++) . schmlCodGen e . (')':)
schmlCodGen (Let1 e1 e2) = ("(let ("++) . schmlCodeClsGen e1 . (")\n"++) . schmlCodGen e2 . (')':)
schmlCodGen (Letrec1 e1 e2) = ("(letrec ("++) . schmlCodeClsGen e1 . (")\n"++) . schmlCodGen e2 . (')':)
schmlCodGen (GRef1 e) = ("(gbox " ++) . schmlCodGen e . (')':)
schmlCodGen (GDeRef1 e) = ("(gunbox " ++) . schmlCodGen e . (')':)
schmlCodGen (GAssign1 e1 e2) = ("(box-set!"++) . schmlCodGen e1 . (' ':) . schmlCodGen e2 . (')':)
schmlCodGen (As1 e t) = ("(: "++) . schmlCodGen e . (' ':) . ((schmlTyGen t ++ ")")++)

schmlCodeClsGen :: [(Name,Type,Exp1)] -> String -> String
schmlCodeClsGen [] = id
schmlCodeClsGen ((x,t,e):e') = ('[':) . ((x++" : " ++ schmlTyGen t ++ " ")++) . schmlCodGen e . ("]"++) . schmlCodeClsGen e'
                                 
schmlTyGen :: Type -> String
schmlTyGen Dyn = "Dyn"
schmlTyGen IntTy = "Int"
schmlTyGen BoolTy = "Bool"
schmlTyGen (FunTy t1 t2) = "(" ++ schmlTyGen t1 ++ " -> " ++ schmlTyGen t2 ++ ")"
schmlTyGen (GRefTy t) = "GRef " ++ schmlTyGen t

nAnnotize :: Exp1 -> IO ()
nAnnotize e = let testDirName = "test/" in createDirectoryIfMissing False testDirName >>
  mapWrite 0 testDirName (map (`schmlCodGen` "") $ nAnnotize' e)
  where mapWrite _ _ [] = return ()
        mapWrite n p (s:s') = writeFile (p ++ show n ++ ".schml") (s ++ "\n") >> mapWrite (n+1) p s'
