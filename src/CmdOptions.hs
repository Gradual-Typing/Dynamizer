module CmdOptions where

import Options.Applicative
import Data.Semigroup ((<>))

data Options = Options
  { sourceFilePath :: FilePath
  , samplesCount   :: Int
  , binsCount      :: Double}

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
         <> metavar "BINS" )
