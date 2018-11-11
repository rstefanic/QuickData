{-# LANGUAGE OverloadedStrings #-}

module QuickData.Main 
  ( main 
  ) where
    
import Data.Semigroup ((<>))
import qualified Data.Text.IO as DTI (writeFile)
import Options.Applicative 
  ( (<**>)
  , Parser
  , helper
  , optional
  , strOption
  , long
  , info
  , short
  , metavar
  , help
  , execParser
  , fullDesc
  , progDesc
  , header
  )

import qualified QuickData.Sql   as SQL
import qualified QuickData.Parse as Parse

newtype Options = Options { outputFile :: Maybe String }

options :: Parser Options
options = Options <$> 
       optional 
     ( strOption
     $ long "output"
    <> short 'o'
    <> metavar "FILE"
    <> help "Output the statement into a text file." 
     )

main :: IO ()
main = do 
    userOpts <- execParser opts
    config <- Parse.getConfig Nothing
    insertStatement <- SQL.insertValues config 
    case outputFile userOpts of 
      Nothing   -> print insertStatement
      Just file -> DTI.writeFile file insertStatement

    where opts = info (options <**> helper)
                   ( fullDesc 
                  <> progDesc "Generate random data for SQL based on a configuration."
                  <> header "QuickData -- Generate Quick Data for SQL")
