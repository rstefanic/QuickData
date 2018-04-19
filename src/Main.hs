{-# LANGUAGE OverloadedStrings #-}

module Main where

import QuickData.Randomize
import QuickData.Types
import QuickData.Sql

import           Dhall
import qualified Data.Text       as T
import qualified Data.ByteString as B
import           Data.Text.IO (writeFile) 

exampleTable :: Table 
exampleTable = 
  Table { metaData = MetaData { tableName = "test_table"
                              , rowCount = 4 
                              }
        , columns = [ Column { columnName = "first_name"
                             , columnType = SqlChar (Size 50) (Just Name)
                             , allowNull  = True 
                             }  
                    , Column { columnName = "last_name"
                             , columnType = SqlVarChar (Size 50) (Just DictWords)
                             , allowNull  = False 
                             }
                    ]
        }

main :: IO ()
main = insertValues exampleTable >>= \x -> print x
  -- Testing for Random Functions
  -- name <- randomBigInt
  -- word <- randomDateTime
  -- print name
  -- print word
  -- x <- input auto "./config"
  -- print (x :: Example)
