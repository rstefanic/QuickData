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

data Options = Options 
    { outputFile :: Maybe String 
    , inputFile  :: Maybe String
    }

options :: Parser Options
options = Options <$> 
        optional 
            ( strOption
            $ long "output"
            <> short 'o'
            <> metavar "FILE"
            <> help "Output the statement into a text file." 
            )
        <*> optional
            ( strOption
            $ long "input"
            <> short 'i'
            <> metavar "FILE"
            <> help "Inpput file for SQL to be generated."
            )

main :: IO ()
main = do 
    userOpts <- execParser opts
    config <- Parse.getConfig $ inputFile userOpts
    insertStatement <- SQL.insertValues config 
    case outputFile userOpts of 
        Nothing   -> print insertStatement
        Just file -> DTI.writeFile file insertStatement

    where opts = info (options <**> helper)
                   ( fullDesc 
                  <> progDesc "Generate random data for SQL based on a configuration."
                  <> header "QuickData -- Generate Quick Data for SQL")
