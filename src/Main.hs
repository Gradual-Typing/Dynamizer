{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Main where

import System.Environment (getArgs)
import System.IO (openFile,hPutStrLn,IOMode(AppendMode),hClose)
import System.Directory (createDirectoryIfMissing)
import Control.Monad (foldM_,liftM,join,void)
import Data.Foldable (forM_)
import Text.Printf (hPrintf)
import Control.Arrow((&&&))
import Data.Function (on)
import Data.List (sortBy,groupBy,find)
import Data.Maybe (isJust)
import Data.Random.Source.DevRandom
import Data.Random.Extras (sample)
import Data.Random (runRVar)
import Data.Interval(interval,member,Interval,Extended(..))

import Parser
import Annotizer
import CodeGen
import L1

writeLattice :: Pretty p => String -> [(Double,p)] -> IO ()
writeLattice dname dps =
  createDirectoryIfMissing False dname >>
  foldM_ (\n (d,p)-> do
            h <- openFile (dname ++ show n ++ ".schml") AppendMode
            hPrintf h ";; %.2f %% \n" (100*d)
            hPutStrLn h (codeGen p)
            hClose h
            return (n+1)) 0 dps

process :: String -> IO ()
process fn = do
  x <- genLattice fn
  forM_ x (writeLattice (fn ++ "/"))

processWithSampling :: String -> Extended Double -> Int -> IO ()
processWithSampling fn nb ns = do
  x <- genLattice fn
  case x of
    Nothing -> return ()
    Just ps -> samplePreciseness nb ns ps >>= writeLattice (fn ++ "/")

processWithExactPreciseness :: String -> Double -> Double -> Int -> IO ()
processWithExactPreciseness fn r1 r2 n = do
  x <- genLattice fn
  case x of
    Nothing -> return ()
    Just ps -> sampleExactPreciseness r1 r2 n ps >>= writeLattice (fn ++ "/")

samplePreciseness :: Gradual p => Extended Double -> Int -> [(Double,p)] -> IO [(Double,p)]
samplePreciseness g n ps =
  liftM join $ mapM ((`runRVar` DevRandom) . sample n) $ groupBy (group g `on` fst) $ sortBy (compare `on` fst) ps

sampleExactPreciseness :: Gradual p => Double -> Double -> Int -> [(Double,p)] -> IO [(Double,p)]
sampleExactPreciseness r1 r2 n ps = (`runRVar` DevRandom) . sample n $ filter (\(a,_) -> a > r1 && a < r2) ps

group :: Extended Double -> Double -> Double -> Bool
group n d1 d2 = isJust $ find (\a -> member d1 a && member d2 a) $ genIntervals n (1 / n)

genIntervals :: Extended Double -> Extended Double -> [Interval Double]
genIntervals 1 l = [interval (0,True) (l,True)]
genIntervals n l = interval ((n-1)*l,False) (n*l,True):genIntervals (n-1) l

genLattice :: String -> IO (Maybe [(Double,L1)])
genLattice fn = do
  x <- parse fn
  case x of
    Nothing -> return Nothing
    Just (e,b) -> return (Just (map (dynamic b &&& id) (lattice e)))

parse :: String -> IO (Maybe (L1,Int))
parse fn = do
  p <- readFile (fn ++ ".schml")
  case parser p of
    Left err -> print err >> return Nothing
    Right e -> let (a,b) = count e
               in putStrLn ("There are " ++ show a ++
                            " less precisely typed programs and " ++
                            show b ++ " type constructors") >> return (Just (e,b))

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fn,r1,r2,ns] -> processWithExactPreciseness fn (read r1::Double) (read r2::Double) (read ns::Int)
    [fn,nb,ns] -> processWithSampling fn (Finite (read nb::Double)) (read ns::Int)
    [fn,fg] -> if fg == "dry" then void (parse fn) else process fn
    _ -> print "Wrong number of arguments\n"
