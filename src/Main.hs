{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Main where

import System.Environment (getArgs)
import System.IO (openFile,hPutStrLn,IOMode(AppendMode),hClose)
import System.Directory (createDirectoryIfMissing)
import Control.Monad (foldM_)
import Text.Printf (hPrintf)
import Data.List (transpose,nub,zipWith5,elemIndices)
import Data.Map.Strict (fromList)
import System.Random
import System.Random.Shuffle(shuffle')
import System.Random.TF (TFGen, seedTFGen)
import Data.IntegerInterval(interval,IntegerInterval,lowerBound,upperBound,Extended(..))
import Control.Monad.State.Lazy (evalState,runState)


import Parser
import Annotizer
import CodeGen
import L1



parse :: String -> IO (Maybe (L1,Int))
parse fn = do
  p <- readFile (fn ++ ".schml")
  case parser p of
    Left err -> print err >> return Nothing
    Right e -> let (a,b) = count e
               in putStrLn ("There are " ++ show a ++
                            " less precisely typed programs and " ++
                            show b ++ " type constructors") >> return (Just (e,b))

randomList :: (Random a) => (a,a) -> Int -> TFGen -> [a]
randomList bnds n = take n . randomRs bnds

writeLattice :: (Gradual p, Pretty p) => Int -> String -> [p] -> IO ()
writeLattice b dname dps =
  createDirectoryIfMissing False dname >>
  foldM_ (\n p -> do
            h <- openFile (dname ++ show n ++ ".schml") AppendMode
            hPrintf h ";; %.2f %% \n" (100*dynamic b p)
            hPutStrLn h (codeGen p)
            hClose h
            return (n+1)) 0 dps

-- I am not happy with this sampling methodology for small programs
genSample:: Int -> [Int] -> L2 -> [L1]
genSample ns tns e = map (pick e) $ nub $ transpose $ zipWith5 (\n a b c d -> randomList (0,n-1) ns $ seedTFGen (a,b,c,d)) tns [0..] [11..] [22..] [33..]

genIntervals :: Double -> Double -> [IntegerInterval]
genIntervals n l = if n > 1
                   then interval (Finite $ ceiling $ (n-1)*l,False) (f $ n*l,True):genIntervals (n-1) l
                   else [interval (0,True) (f l,True)]
  where f = Finite . floor

-- program, # samples / bin, # bins
sample :: L1 -> Int -> Double -> [[L1]]
sample prog nb b =
  let (l,m) = runState (countTypeLattice undefined prog) 0
      i         = genIntervals b $ fromIntegral m/b
  in sampleMN prog l m i nb
     
  where
    -- index,# of nodes,staticallity,lattice -> potential -> current -> generator -> desired min -> desired max -> types
    sampleOne :: [(SourcePos,Int,[Int],[Type])] -> Int -> Int -> TFGen -> Int -> Int -> [(SourcePos,Type)]
    sampleOne [] _ _ _ _ _ = []
    sampleOne ((i,n,s,ts):l) p c g mn mx =
      let p'       = p-n 
          (c',g')  = randomR (max 0 (mn-p'-c),min n (mx-c)) g
          sc       = elemIndices c' s
      in if null sc || p <= 0
         then []
         else let (xi,g'') = randomR (0,length sc-1) g'
              in (i,ts !! (sc !! xi)):sampleOne l p' (c'+c) g'' mn mx
    
    -- program -> info -> max # nodes -> interval -> # samples -> programs
    sampleN :: L1 -> [(SourcePos,Int,[Int],[Type])] -> Int -> IntegerInterval -> Int -> [L1]
    sampleN _ _ _ _ 0  = []
    sampleN p l1 m i n = case (lowerBound i,upperBound i) of
      (Finite lb,Finite ub) -> (replaceTypes (fromList (sampleOne (shuffle' l1 (length l1) (seedTFGen (0, 11, 22, 33))) m 0 (seedTFGen (0, 11, 22, 33)) (fromInteger lb) (fromInteger ub))) p):sampleN p l1 m i (n-1)
      _ -> error "internal error: unsupported ranges"

    sampleMN :: L1 -> [(SourcePos,Int,[Int],[Type])] -> Int -> [IntegerInterval] -> Int -> [[L1]]
    sampleMN _ _ _ [] _ = [[]]
    sampleMN p l1 m (i:is) n =  sampleN p l1 m i n:sampleMN p l1 m is n

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fn] -> do
      x <- parse fn
      case x of
        Nothing -> return ()
        Just (e,w) -> writeLattice w (fn ++ "/") $ lattice e
    [fn, ns] -> do
      x <- parse fn
      case x of
        Nothing -> return ()
        Just (e,w) -> writeLattice w (fn ++ "/") $ genSample (read ns::Int) (map (\(_,c,_,_)-> c) $ evalState (countTypeLattice undefined e) 0) (localLattice e)
     -- path, number of samples in each bin, number of bins
    [fn, ns, nb] -> do
      x <- parse fn
      case x of
        Nothing -> return ()
        Just (e,w) -> writeLattice w (fn ++ "/") $ concat $ sample e (read ns::Int) (read nb::Double)
    _ -> print "Wrong number of arguments"
