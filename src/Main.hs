{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}


module Main where

import System.Environment (getArgs)
import System.IO (openFile,hPutStrLn,IOMode(AppendMode),hClose)
import System.Directory (createDirectoryIfMissing)
import Control.Monad (void)
import Text.Printf (hPrintf)
import Control.Arrow((&&&))
import Data.List (sort)
import System.Random
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

parse :: String -> IO (Maybe (L1,Int,Integer))
parse fn = do
  p <- readFile (fn ++ ".schml")
  case parser p of
    Left err -> print err >> return Nothing
    Right e -> let (a,b) = count e
               in putStrLn ("There are " ++ show a ++
                            " less precisely typed programs and " ++
                            show b ++ " type constructors") >> return (Just (e,b,a))

randomList :: (Random a) => (a,a) -> Int -> StdGen -> [a]
randomList bnds n = take n . randomRs bnds

-- splitBigInt :: Integer -> [Int]
-- splitBigInt n = undefined

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fn] -> void (parse fn)
    [fn, ns] -> do
      x <- parse fn
      case x of
        Nothing -> return ()
        Just (e,b,a) -> do
          seed  <- newStdGen
          let rs = sort $ randomList (1,a) (read ns::Int) seed
          putStrLn $ "Random indices: " ++ show rs
          createDirectoryIfMissing False fn
          source e $$ conduit b =$ sink (fn ++ "/") 0 rs
    _ -> print "Wrong number of arguments\n"
