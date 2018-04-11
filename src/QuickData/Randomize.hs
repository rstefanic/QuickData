{-# LANGUAGE OverloadedStrings #-}

module QuickData.Randomize (
     randomDictWord
   , randomName
   , randomWord
   , randomTinyInt
   , randomSmallInt
   , randomInt
   , randomBigInt
   , randomDouble
   , randomFloat
   , randomBit
   , randomByte
   , randomDateTime
   , Words
   ) where

import           QuickData.Types

import           Data.Char
import           Data.DateTime
import qualified Data.ByteString.Lazy  as B
import qualified Data.ByteString.Char8 as C
import           System.Random  (random, randomIO, randomRIO)

dict :: IO Words
dict = do
  dict <- readFile "data/dict.txt"
  return (Words $ lines dict)

names :: IO Words
names = do
  names <- readFile "data/names.txt"
  return (Words $ lines names)

randomName :: IO String
randomName = names >>= randomWord

randomDictWord :: IO String
randomDictWord = dict >>= randomWord

randomWord :: Words -> IO String
randomWord (Words words) = do 
  randomIndex <- randomRIO (0, length words - 1)
  return $ filter (/= '\'') $ words !! randomIndex

randomTinyInt :: IO Int
randomTinyInt = randomRIO (0, 255)

randomSmallInt :: IO Int
randomSmallInt = randomRIO (-32768, 32767)

randomInt :: IO Int
randomInt = randomRIO (-2147483648, 2147483647)

randomBigInt :: IO Integer
randomBigInt = randomRIO (-9223372036854775808, 9223372036854775807)

randomDouble :: IO Double 
randomDouble = randomIO :: IO Double

randomFloat :: IO Float 
randomFloat = randomIO :: IO Float

randomBit :: IO Int
randomBit = randomRIO (0, 1)

randomByte :: IO C.ByteString
randomByte = do
  n <- randomRIO (0, 255)
  return $ C.singleton $ chr n

randomDateTime :: IO DateTime
randomDateTime = do
  n <- randomRIO (0, maxBound :: Int) 
  return $ fromSeconds (toInteger n)