{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ExistentialQuantification #-}

module QuickData.Randomize 
  ( TextResult
  , name 
  , buildUTF8Texts
  , buildUnicodeTexts
  , randomizeFromRange 
  , bigInt
  , double
  , float
  , bit
  , byte
  , dateTime
  ) where

import           Prelude hiding (min, max)
import           Control.Monad.State.Lazy
import           Data.Char
import qualified Data.ByteString.Char8 as C
import           Data.DateTime
import           System.Random  (randomIO, randomRIO)

import           QuickData.Internal

type TextResult = StateT (Int, [String]) IO [String]

buildUTF8Texts :: Integer -> TextResult
buildUTF8Texts max = StateT $ 
    \(currentLength, str) -> do 
        word <- liftIO utf8DictWord
        if length word + currentLength < fromInteger max
          then runStateT (buildUTF8Texts max) (currentLength + length word, str ++ [word])
          else return (str, (currentLength, str))

buildUnicodeTexts :: Integer -> TextResult
buildUnicodeTexts max = StateT $ 
    \(currentLength, str) -> do
        word <- liftIO unicodeDictWord
        if length word + currentLength < fromInteger max
          then runStateT (buildUnicodeTexts max) (currentLength + length word, str ++ [word])
          else return (str, (currentLength, str))

name :: Size -> IO String
name Max          = getName >>= \x -> return $ cutoffLength x 8000
name (Size _ max) = getName >>= \x -> return $ cutoffLength x max

-- | Text Building Helpers

-- withinLength :: String -> Integer -> Bool
-- withinLength str i | length str < fromInteger i = True
--                    | otherwise      = False

cutoffLength :: String -> Integer -> String
cutoffLength str max = cutoffLength' str 0
  where cutoffLength' []     _ = []
        cutoffLength' (x:xs) n | n < max   = x : cutoffLength' xs (n + 1)
                               | otherwise = [x]

newtype Words = Words [String] deriving (Eq, Show)

utf8Dict :: IO Words
utf8Dict = do
  wl <- readFile "data/dict.txt"
  return (Words $ lines wl)

unicodeDict :: IO Words
unicodeDict = do
  wl <- readFile "data/greek.txt"
  return (Words $ lines wl)

names :: IO Words
names = do
  nl <- readFile "data/names.txt"
  return (Words $ lines nl)

getName :: IO String
getName = names >>= getWord

utf8DictWord :: IO String
utf8DictWord = utf8Dict >>= getWord

unicodeDictWord :: IO String
unicodeDictWord = unicodeDict >>= getWord

getWord :: Words -> IO String
getWord (Words wl) = do 
  randomIndex <- randomRIO (0, length wl - 1)
  return $ filter (/= '\'') $ wl !! randomIndex

randomizeFromRange :: Size -> IO Integer
randomizeFromRange (Size min max) = randomRIO (min, max)
randomizeFromRange Max            = randomRIO (0, 8000)

bigInt :: IO Integer
bigInt = randomRIO bigIntRange

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
