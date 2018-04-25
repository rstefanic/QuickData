{-# LANGUAGE OverloadedStrings #-}

module QuickData.Main 
  ( main 
  ) where

import           QuickData.Sql
import qualified QuickData.Parse as Parse

--import           Data.Text.IO (writeFile) 

main :: IO ()
main = do 
  config <- Parse.getConfig 
  insertStatement <- insertValues config 
  print insertStatement