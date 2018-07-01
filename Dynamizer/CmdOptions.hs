module CmdOptions where

import           Data.Semigroup      ((<>))
import           Options.Applicative

data Options = Options
  { sourceFilePath :: FilePath
  , samplesCount   :: Int
  , binsCount      :: Double
  , coarseGrained  :: Bool
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
      <*> switch
          ( long "coarse"
         <> help "Enable coarse grained lattice"
         <> showDefault)
      <*> switch
          ( long "fine"
         <> help "Enable fine grained lattice. It is not feasible for programs with many type annotations."
         <> showDefault)
