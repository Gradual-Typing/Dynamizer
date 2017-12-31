

module Main where

import           Control.Monad       (foldM_)
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


import           CmdOptions
import           CodeGen
import           Lattice
import           Parser
import           Sampling

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
greet (Options srcFilePath ns nb) = do
  p <- readFile srcFilePath
  case parser p of
    Left err -> print err
    Right e -> do
      let (a,w')  = count e
          w       = getSum w'
          dirPath = dropExtension srcFilePath ++ "/"
      putStrLn ("There are " ++ show (getProduct a) ++ " less precisely typed programs and " ++ show w ++ " type constructors")
      writeLattice w dirPath $ samplingMode a e
  where samplingMode a e
          | ns < 0 = DL.toList $ lattice e
          | nb == 1 = if a > 10000
            then sampleUniformally e ns
            else sampleUniformally' e ns
          | otherwise = concat $ sampleFromBins e ns nb

main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
     <> progDesc "Generates gradually-typed configurations of a statically-typed Grift program"
     <> header "Dynamizer - lattice generator for gradual typing" )
