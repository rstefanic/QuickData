{-# LANGUAGE OverloadedStrings #-}

module QuickData.Randomize 
  ( name 
  , buildTexts
  , tinyInt
  , smallInt
  , int
  , bigInt
  , double
  , float
  , bit
  , byte
  , dateTime
  ) where

import           Prelude hiding (max)
import           Control.Monad.State.Lazy
import           Data.Char
import qualified Data.ByteString.Char8 as C
import           Data.DateTime
import           System.Random  (randomIO, randomRIO)

import           QuickData.Internal

type TextResult = StateT (Int, [String]) IO [String]

buildTexts :: Integer -> TextResult
buildTexts max = StateT $ 
    \(currentLength, str) -> do 
        word <- liftIO dictWord
        if length word + currentLength < fromInteger max
        then runStateT (buildTexts max) (currentLength + length word, str ++ [word])
        else return (str, (currentLength, str))

name :: Size -> IO String
name Max      = getName >>= \x -> return $ cutoffLength x 8000
name (Size n) = getName >>= \x -> return $ cutoffLength x n

-- | Text Building Helpers
withinLength :: String -> Integer -> Bool
withinLength str i | length str < fromInteger i = True
                   | otherwise      = False

cutoffLength :: String -> Integer -> String
cutoffLength str max = cutoffLength' str 0
  where cutoffLength' []     _ = []
        cutoffLength' (x:xs) n | n < max   = x : cutoffLength' xs (n + 1)
                               | otherwise = [x]

newtype Words = Words [String] deriving (Eq, Show)

dict :: IO Words
dict = do
  wl <- readFile "data/dict.txt"
  return (Words $ lines wl)

names :: IO Words
names = do
  nl <- readFile "data/names.txt"
  return (Words $ lines nl)

getName :: IO String
getName = names >>= getWord

dictWord :: IO String
dictWord = dict >>= getWord

getWord :: Words -> IO String
getWord (Words wl) = do 
  randomIndex <- randomRIO (0, length wl - 1)
  return $ filter (/= '\'') $ wl !! randomIndex

tinyInt :: IO Int
tinyInt = randomRIO (0, 255)

smallInt :: IO Int
smallInt = randomRIO (-32768, 32767)

int :: IO Int
int = randomRIO (-2147483648, 2147483647)

bigInt :: IO Integer
bigInt = randomRIO (-9223372036854775808, 9223372036854775807)

double :: IO Double 
double = randomIO :: IO Double

float :: IO Float 
float = randomIO :: IO Float

bit :: IO Int
bit = randomRIO (0, 1)

byte :: IO C.ByteString
byte = do
  n <- randomRIO (0, 255)
  return $ C.singleton $ chr n

dateTime :: IO DateTime
dateTime = do
  n <- randomRIO (0, maxBound :: Int) 
  return $ fromSeconds (toInteger n)