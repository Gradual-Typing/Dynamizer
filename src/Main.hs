{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Main where

import System.Environment (getArgs)
import System.IO (openFile,hPutStrLn,IOMode(AppendMode),hClose)
import System.Directory (createDirectoryIfMissing)
import Control.Monad (foldM_, when)
import Text.Printf (hPrintf)
import Control.Arrow ((&&&))

import Parser
import Annotizer
import CodeGen

writeLattice :: String -> [(String,Double)] -> IO ()
writeLattice dname es =
  createDirectoryIfMissing False dname >>
  foldM_ (\n (s1,s2)-> do
            h <- openFile (dname ++ show n ++ ".schml") AppendMode
            hPrintf h ";; %.2f %% \n" (100*s2)
            hPutStrLn h s1
            hClose h
            return (n+1))
  0 es

process :: String -> String -> String -> IO ()
process d f s =
  let r = parser s in
  case r of
    Left err -> print err
    Right e -> putStrLn ("There are " ++ show (count e) ++ " variants!")
               >> when (f == "g") (writeLattice d (map (codeGen &&& static) $ lattice e))

processFile :: String -> String -> IO ()
processFile f n = readFile (n ++ ".schml") >>= process (n ++ "/") f

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname,flag] -> processFile flag fname
    _ -> print "Wrong number of arguments\n"
