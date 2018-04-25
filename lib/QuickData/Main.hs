{-# LANGUAGE OverloadedStrings #-}

module QuickData.Main 
  ( main 
  ) where

import           QuickData.Sql
import qualified QuickData.Parse as Parse

main :: IO ()
main = do 
  config <- Parse.getConfig 
  insertStatement <- insertValues config 
  print insertStatement