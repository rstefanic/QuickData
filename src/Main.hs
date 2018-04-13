{-# LANGUAGE OverloadedStrings #-}

module Main where

import QuickData.Randomize
import QuickData.Types
import QuickData.Sql

import           Dhall
import           Data.Text       as T
import qualified Data.ByteString as B

exampleTable :: Table 
exampleTable = 
  Table { metaData = MetaData { tableName = "test_table"
                              , rowCount = 4 
                              }
        , columns = [ Column { columnName = "first_name"
                             , columnType = SqlBit
                             , maxLength  = Just 20
                             , allowNull  = True 
                             }  
                    , Column { columnName = "last_name"
                             , columnType = SqlSmallInt
                             , maxLength  = Just 25
                             , allowNull  = False 
                             }
                    ]
        }

main :: IO ()
main = print $ insertValues exampleTable
  -- Testing for Random Functions
  -- name <- randomBigInt
  -- word <- randomDateTime
  -- print name
  -- print word
  -- x <- input auto "./config"
  -- print (x :: Example)
