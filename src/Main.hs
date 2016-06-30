{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}


module Main where

import System.Environment (getArgs)
import System.IO (openFile,hPutStrLn,IOMode(AppendMode),hClose)
import System.Directory (createDirectoryIfMissing)
import Control.Monad (foldM_)
import Text.Printf (hPrintf)
import Control.Arrow((&&&))
import Data.List (transpose,nub,zipWith5)
import System.Random
import System.Random.TF (TFGen, seedTFGen)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Monad.IO.Class (liftIO)


import Parser
import Annotizer
import CodeGen
import L1

source :: Gradual p => p -> Source IO p
source = CL.sourceList . lattice

conduit :: (Gradual p, Pretty p) => Int -> Conduit p IO (Double,String)
conduit !b = CL.map (dynamic b &&& codeGen)

sink :: String -> Int -> [Integer] -> Sink (Double,String) IO ()
sink _ _ [] = liftIO $ putStrLn "\ndone."
sink dname n (i:xs) = do
  CL.drop $ fromIntegral i
  x <- await
  case x of
    Nothing -> return ()
    Just (!d,!p) -> do
      h <- liftIO $ openFile (dname ++ show n ++ ".schml") AppendMode
      liftIO $ hPrintf h ";; %.2f %% \n" (100*d)
      liftIO $ hPutStrLn h p
      liftIO $ hClose h
      sink dname (n+1) $ map (i-) xs

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
        Just (e,w) -> writeLattice w (fn ++ "/") $ map (pick e) $ nub $ transpose $ zipWith5 (\n a b c d -> randomList (0,n-1) (read ns::Int) $ seedTFGen (a,b,c,d)) (countTypeLattice e) [0..] [11..] [22..] [33..]
    _ -> print "Wrong number of arguments\n"
