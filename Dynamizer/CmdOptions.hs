module CmdOptions where

import           Data.Semigroup      ((<>))
import           Options.Applicative

data Options = Options
  { sourceFilePath          :: FilePath
  , fineGrained             :: Bool
  , samplesCount            :: Int
  , binsCount               :: Double
  , coarseGrained           :: Bool
  , modulesCount            :: Int }

options :: Parser Options
options = Options
      <$> argument str
          ( metavar "FILE"
         <> help "File path of a grift program" )
      <*> switch
          ( long "fine"
         <> help "Enable fine grained lattice. It is not feasible for programs with many type annotations."
         <> showDefault)
      <*> option auto
          ( long "samples"
         <> metavar "SAMPLES"
         <> value (-1)
         <> help "Number of samples" )
      <*> option auto
          ( long "bins"
         <> help "Number of bins"
         <> showDefault
         <> value 1
         <> metavar "BINS")
      <*> switch
          ( long "coarse"
         <> help "Enable coarse grained lattice over modules"
         <> showDefault)
      <*> option auto
          ( long "modules-count"
         <> help "Number of modules to be auto detected in case no syntactic module appear in input source"
         <> showDefault
         <> value (-1)
         <> metavar "MODULES")
