{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Main where

import System.Environment (getArgs)
import System.IO (openFile,hPutStrLn,IOMode(AppendMode),hClose)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (dropExtensions)
import Control.Monad (foldM_, when)
import Text.Printf (hPrintf)
import Control.Arrow ((&&&))

import Parser
import Annotizer
import CodeGen

writeLattice :: String -> [(String,Double)] -> IO ()
writeLattice dname es =
  createDirectoryIfMissing False dname >>
  foldM_ (\n (s1,s2)->
            let path = (dname ++ show n ++ ".schml")
            in openFile path AppendMode >>= \h ->
            hPrintf h ";; %.2f %% \n" (100*s2)
            >> hPutStrLn h s1
            >> hClose h
            >> return (n+1))
  0 es

process :: String -> String -> String -> IO ()
process dname flag source = do
  let res = parser source
  case res of
    Left err -> print err
    Right e -> putStrLn ("There are " ++ show (count e) ++ " gradually-typed version of this program!")
               >> when (flag == "g") (writeLattice dname (map (codeGen &&& static) $ lattice e))

processFile :: String -> String -> IO ()
processFile flag fname = readFile fname >>= process (dropExtensions fname ++ "/") flag

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> print "No arguments passed\n"
    [fname,flag] -> processFile flag fname
    _ -> print "Wrong number of arguments\n"
