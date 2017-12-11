{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Main where

import System.Environment (getArgs)
import System.IO (openFile,hPutStrLn,IOMode(WriteMode),hClose)
import System.Directory (createDirectoryIfMissing,removePathForcibly)
import System.FilePath (dropExtension)
import Text.Printf (hPrintf)
import Control.Monad (unless,foldM_)
import Data.Monoid (Sum(..), Product(..))


import Parser
import Annotizer
import CodeGen
import Sampling

writeLattice :: (Gradual p, Pretty p) => Int -> String -> [p] -> IO ()
writeLattice b dname dps =
  removePathForcibly dname >> createDirectoryIfMissing False dname >>
  foldM_ (\n p -> do
            h <- openFile (dname ++ show n ++ ".grift") WriteMode
            hPrintf h ";; %.2f%%\n" (100 * (fromIntegral $ getSum (static p) :: Double) / fromIntegral b)
            hPutStrLn h (codeGen p)
            hClose h
            return (n+1)) (0 :: Int) dps

main :: IO ()
main = do
  args <- getArgs
  unless (not (null args)) (putStrLn "You must provide the path to the Schml source file")
  let srcFilePath = head args
  p <- readFile srcFilePath
  case parser p of
    Left err -> print err
    Right e -> do
      let (a,w')  = count e
          w       = getSum w'
          dirPath = dropExtension srcFilePath ++ "/"
      putStrLn ("There are " ++ show (getProduct a) ++ " less precisely typed programs and " ++ show w ++ " type constructors")
      case tail args of
        [] -> writeLattice w dirPath $ lattice e
        [ns] -> writeLattice w dirPath $
          if a > 10000
          then sampleUniformally e (read ns::Int)
          else sampleUniformally' e (read ns::Int)
        [ns, nb] -> writeLattice w dirPath $ concat $ sampleFromBins e (read ns::Int) (read nb::Double)
        _ -> putStrLn "Invalid number of arguments"
