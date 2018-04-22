{-# LANGUAGE OverloadedStrings #-}

module QuickData.Main 
  ( main 
  ) where

-- import           QuickData.Randomize
-- import           QuickData.Internal
-- import           QuickData.Sql
import qualified QuickData.Parse as Parse

--import           Data.Text.IO (writeFile) 

main :: IO ()
main = do 
  config <- Parse.getConfig 
  print config