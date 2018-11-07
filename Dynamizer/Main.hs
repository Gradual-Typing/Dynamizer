module Main where

import           Control.Arrow       ((***))
import           Control.Exception   (AssertionFailed (..), throwIO)
import           Control.Monad       (foldM_, when)
import qualified Data.DList          as DL
import           Data.Monoid         (Product (..), Sum (..))
import           Data.Semigroup      ((<>))
import           Options.Applicative (execParser, fullDesc, header, helper,
                                      info, progDesc, (<**>))
import           System.Directory    (createDirectoryIfMissing,
                                      removePathForcibly)
import           System.FilePath     (dropExtension)
import           System.IO           (IOMode (WriteMode), hClose, hPutStrLn,
                                      openFile)
import           Text.Printf         (hPrintf)

import           Language.Grift.Common.Syntax
import           Language.Grift.Common.Pretty
import           Language.Grift.Source.Pretty ()
import           Language.Grift.Source.Parser

import           CmdOptions
import           Dynamizer.Lattice
import           Dynamizer.Sampling

writeLattice :: (Gradual p, Pretty p) => Int -> String -> [p] -> IO ()
writeLattice b dname dps =
  removePathForcibly dname >> createDirectoryIfMissing False dname >>
  foldM_ (\n p -> do
            h <- openFile (dname ++ show n ++ ".grift") WriteMode
            hPrintf h ";; %.2f%%\n" (100 * (fromIntegral $ getSum (static p) :: Double) / fromIntegral b)
            hPutStrLn h (codeGen p)
            hClose h
            return (n+1)) (0 :: Int) dps

greet :: Options -> IO ()
greet (Options srcFilePath fine ns nb coarse modules) = do
  ast <- parseGriftProgram srcFilePath
  let (annotationLatticeSize,typeConstCount)  = (getProduct *** getSum) $ count ast
      dirPath = dropExtension srcFilePath ++ "/"
  l <- executionMode annotationLatticeSize typeConstCount ast
  writeLattice typeConstCount dirPath l
  where
    executionMode annotationLatticeSize typeConstCount ast
      | coarse = case ast of
                   Script script -> return $ Script <$>
                     if modules > 0
                       then coarseLatticeWithAutoDetectedModules modules script
                       else coarseLattice modules script
                   Modules _ -> do
                     when (modules > 0) $ throwIO $ AssertionFailed "Modules number can not be specified by the user when the input program has modules"
                     return $ DL.toList $ moduleLattice ast
      | fine   = do
       putStrLn ("There are " ++ show annotationLatticeSize ++ " less precisely typed programs and " ++ show typeConstCount ++ " type constructors")
       fineMode annotationLatticeSize ast
      | otherwise = throwIO $ AssertionFailed "fine or coarse switches are expected but none are provided"

    fineMode annotationLatticeSize ast | ns == -1 && nb == 1 = return $ DL.toList $ lattice ast
                                       | nb == 1 && annotationLatticeSize > 10000 = return $ sampleUniformally ast ns
                                       | nb == 1 = return $ sampleUniformally' ast ns
                                       | otherwise = concat <$> sampleFromBins ast ns nb
main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Generates gradually-typed configurations of a typed Grift program"
     <> header "Dynamizer - lattice generator for gradual typing" )
