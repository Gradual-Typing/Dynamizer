{-# LANGUAGE LambdaCase #-}
module Annotizer where

import System.Directory (createDirectoryIfMissing)
import Control.Monad (liftM, liftM2, liftM3)

import Syntax
import TRecon

annotize :: Exp1 -> IO Exp2
annotize e = teval e >>= \case Right (e',_) -> return e'

nAnnotize' :: Exp2 -> [Exp2]
nAnnotize' (Op2 op e1 e2) = liftM2 (Op2 op) (nAnnotize' e1) $ nAnnotize' e2
nAnnotize' (If2 e1 e2 e3) =
  liftM3 If2 (nAnnotize' e1) (nAnnotize' e2) $ nAnnotize' e3
nAnnotize' (App2 e1 e2) =
  liftM2 App2 (nAnnotize' e1) $ nAnnotize' e2
nAnnotize' (Lam2 (x,t1) (e,t2)) =
  nAnnotizeTy' t1 >>= \t1' -> nAnnotizeTy' t2 >>= \t2' -> nAnnotize' e >>= \e' -> return $ Lam2 (x,t1') (e',t2')
nAnnotize' (Ref2 e) = liftM Ref2 $ nAnnotize' e
nAnnotize' (DeRef2 e) = liftM DeRef2 $ nAnnotize' e
nAnnotize' (Assign2 e1 e2) =
  liftM2 Assign2 (nAnnotize' e1) $ nAnnotize' e2
nAnnotize' (Let2 (x,t,e1) e2) =
  nAnnotizeTy' t >>= \t' -> nAnnotize' e1 >>= \e1' -> liftM (Let2 (x,t',e1')) $ nAnnotize' e2
nAnnotize' e = [e]

nAnnotizeTy' :: Type -> [Type]
nAnnotizeTy' (TVar _) = [IntTy,Dyn] -- arbitrary type
nAnnotizeTy' (RefTy t) = Dyn: map RefTy (nAnnotizeTy' t)
nAnnotizeTy' (FunTy t1 t2) = Dyn:[FunTy t1' t2' | t1' <- nAnnotizeTy' t1, t2' <- nAnnotizeTy' t2]
nAnnotizeTy' t = [t,Dyn]

schmlCodGen :: Exp2 -> String -> String
schmlCodGen (N2 n) =  shows n
schmlCodGen (B2 b) | b = ("#t"++)
                 | not b = ("#f"++)
schmlCodGen (Op2 op e1 e2) =
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
schmlCodGen (If2 e1 e2 e3) = ("(if " ++) . schmlCodGen e1 . ("\n"++) . schmlCodGen e2 . ("\n"++) . schmlCodGen e3 . (')':)
schmlCodGen (Var2 x) = (x++)
schmlCodGen (App2 e1 e2) = ('(':) . schmlCodGen e1 . (' ':) . schmlCodGen e2 . (')':)
schmlCodGen (Lam2 (x,t1) (e,t2)) = ("(lambda ("++) . 
                                 (case t1 of
                                   Dyn -> (x++)
                                   _ -> ('[':) . (x++) . (" : "++) . (schmlTyGen t1 ++) . (']':))
                                 . (") : "++) . (schmlTyGen t2 ++) . ("\n"++) . schmlCodGen e . (')':)
                                 
schmlTyGen :: Type -> String
schmlTyGen Dyn = "Dyn"
schmlTyGen IntTy = "Int"
schmlTyGen BoolTy = "Bool"
schmlTyGen (FunTy t1 t2) = "(" ++ schmlTyGen t1 ++ " -> " ++ schmlTyGen t2 ++ ")"
schmlTyGen (RefTy t) = undefined
schmlTyGen (TVar _) = "Int" -- arbitrary type

nAnnotize :: Exp1 -> IO ()
nAnnotize e = let testDirName = "test/" in createDirectoryIfMissing False testDirName >>
  annotize e >>= \e' -> mapWrite 0 testDirName $ map (`schmlCodGen` "") $ nAnnotize' e'
  where mapWrite _ _ [] = return ()
        mapWrite n p (s:s') = writeFile (p ++ show n ++ ".schml") (s ++ "\n") >> mapWrite (n+1) p s'
