{-# LANGUAGE OverloadedStrings #-}

module Main where

import QuickData.Randomize
import QuickData.Types

import           Dhall
import           Data.Text       as T
import qualified Data.ByteString as B

main :: IO ()
main = do 
  -- Testing for Random Functions
  name <- randomBigInt
  word <- randomDateTime
  print name
  print word
  -- x <- input auto "./config"
  -- print (x :: Example)
