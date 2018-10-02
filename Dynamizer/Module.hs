{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE UndecidableInstances #-}

module Dynamizer.Module
  ( computeModules
  , FunName
  ) where

import           Control.Arrow         ((&&&))
import           Control.Monad.State.Lazy (State, runState, get, put)
import qualified Data.DList            as DL
import           Data.Foldable         (fold, foldMap)
import           Data.Function         (on)
import           Data.Graph            (flattenSCC, stronglyConnComp)
import           Data.List             (sortBy)
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromMaybe, fromJust)
import           Data.Monoid           (Sum (..), (<>))

import           Language.Grift.Source.Syntax
import           Language.Grift.Source.Utils


type Key        = Int
type Size       = Sum Int
type FunName    = Name
type FunInfo    = (FunName, (Key, Size))
type CallGraph  = [(FunName, Key, [Key])]
type FunInfoMap = M.Map FunName (Key, Size)

computeModules :: forall a t. Int -> Ann a (ExpF t) -> [[FunName]]
computeModules desiredCount p =
  if modulesCount <= desiredCount
  then modules
  else mergeModules (modulesCount - desiredCount) $ sortBy (compare `on` snd) $ map f modules
  where
    f :: [FunName] -> ([FunName], Size)
    f = id &&& foldMap (snd . fromJust . flip M.lookup funInfo)
    
    mergeModules :: Int -> [([FunName], Size)] -> [[FunName]]
    mergeModules 0 ms       = map fst ms
    mergeModules _ []       = []
    mergeModules _ [(x, _)] = [x]
    mergeModules n (x:y:l)  = mergeModules (n - 1) $ sortBy (compare `on` snd) (x <> y : l)
    
    (cg, funInfo) = buildCallGraph $ annotateFunsWithKeys p

    scc = stronglyConnComp cg

    modules = map flattenSCC scc

    modulesCount = length modules

buildCallGraph :: forall a t. (Ann (a, Maybe Key) (ExpF t), [FunInfo])
               -> (CallGraph, FunInfoMap)
buildCallGraph (p, l) = (DL.toList $ mapAnn Nothing Nothing p, info)
  where
    info = M.fromList l

    findApps :: ExpF t (DL.DList Key) -> DL.DList Key
    findApps (P (Var name)) = case M.lookup name info of
                                (Just (key, _)) -> [key]
                                _               -> []
    findApps e = fold e

    mapAnn :: Maybe FunName
           -> Maybe Key
           -> Ann (a, Maybe Key) (ExpF t)
           -> DL.DList (FunName, Key, [Key])
    mapAnn name _ (Ann (_, k) e) = f name k e

    f :: Maybe FunName
      -> Maybe Key
      -> ExpF t (Ann (a, Maybe Key) (ExpF t))
      -> DL.DList (FunName, Key, [Key])
    f _ k (DLam name _ e _) = [(name, fromJust k, DL.toList $ cata (const findApps) e)]
    f name k (Lam _ e _)    = [(fromJust name, fromJust k, DL.toList $ cata (const findApps) e)]
    f name k e              = foldMap (mapAnn name k) e

annotateFunsWithKeys :: forall a t. Ann a (ExpF t)
                     -> (Ann (a, Maybe Key) (ExpF t), [FunInfo])
annotateFunsWithKeys p = (p', l)
  where
    (p', (_, l)) = runState (mapAnn Nothing p) (0, [])

    mapAnn :: Maybe FunName
           -> Ann a (ExpF t)
           -> State (Key, [FunInfo]) (Ann (a, Maybe Key) (ExpF t))
    mapAnn name (Ann a e) = f name a e

    f :: Maybe FunName
      -> a
      -> ExpF t (Ann a (ExpF t))
      -> State (Key, [FunInfo]) (Ann (a, Maybe Key) (ExpF t))
    f _ a e@(DLam name args b t) = do
      val <- getNewId name e
      return $ Ann (a, Just val) $ DLam name args (bottomUp (\a' _ -> (a', Nothing)) b) t
    f name a e@(Lam args b t)    = do
      -- if it is an anonymous lambda, no other application will be able to call
      -- it, so it does not matter what name we give it
      val <- getNewId (fromMaybe "" name) e
      return $ Ann (a, Just val) $ Lam args (bottomUp (\a' _ -> (a', Nothing)) b) t
    f name a e        = Ann (a, Nothing) <$> traverse (mapAnn name) e

    getNewId :: FunName -> ExpF t (Ann a (ExpF t)) -> State (Key, [FunInfo]) Key
    getNewId name e = do
      (val, ls) <- get
      put (val + 1, (name, (val, cata getExprSize $ Ann err e)):ls)
      return val
        where
          err = error "getNewId: unexpected evaluation of unused annotated information"

getExprSize :: forall a t. a -> ExpF t Size -> Size
getExprSize _ = (+) 1 . fold
