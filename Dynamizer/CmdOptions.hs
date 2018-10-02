module CmdOptions where

import           Data.Semigroup      ((<>))
import           Options.Applicative

data Options = Options
  { sourceFilePath :: FilePath
  , samplesCount   :: Int
  , binsCount      :: Double
  , coarseGrained  :: Int
  , fineGrained    :: Bool}

options :: Parser Options
options = Options
      <$> argument str
          ( metavar "FILE"
         <> help "File path of a grift program" )
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
      <*> option auto
          ( long "coarse"
         <> help "Enable coarse grained lattice over auto detected MODULES modules"
         <> showDefault
         <> value 0
         <> metavar "MODULES")
      <*> switch
          ( long "fine"
         <> help "Enable fine grained lattice. It is not feasible for programs with many type annotations."
         <> showDefault)
