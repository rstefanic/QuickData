{-# LANGUAGE OverloadedStrings #-}

module QuickData.Main 
  ( main 
  ) where

import qualified QuickData.Sql   as SQL
import qualified QuickData.Parse as Parse

main :: IO ()
main = do 
  config <- Parse.getConfig 
  insertStatement <- SQL.insertValues config 
  print insertStatement