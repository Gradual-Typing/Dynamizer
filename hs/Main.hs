{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Main where

import System.Environment(getArgs)
import System.Directory (createDirectoryIfMissing)
import System.FilePath(dropExtensions)
import Control.Monad (foldM_)

import Parser
import Annotizer
import CodeGen

writeLattice :: String -> [String] -> IO ()
writeLattice dname es =
  createDirectoryIfMissing False dname >>
  foldM_ (\n s-> writeFile (dname ++ show n ++ ".schml") (s ++ "\n") >> return (n+1)) 0 es

process :: String -> String -> IO ()
process dname source = do
  let res = parser source
  case res of
    Left err -> print err
    Right e -> writeLattice dname $ map codeGen $ lattice e

processFile :: String -> IO ()
processFile fname = readFile fname >>= process (dropExtensions fname ++ "/")

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> print "No arguments passed\n"
    [fname] -> processFile fname
