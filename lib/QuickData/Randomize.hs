{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ExistentialQuantification #-}

module QuickData.Randomize 
  ( TextResult
  , name 
  , problemString
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
import qualified Data.ByteString.Char8       as C
import           Data.DateTime
import           Data.Monoid
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TI
import qualified Data.Vector                 as V
import           System.Environment ()
import           System.IO
import           System.Random  (randomIO, randomRIO)

import           QuickData.Internal

type TextResult = StateT (Int, T.Text) IO T.Text 

buildUTF8Texts :: Integer -> TextResult
buildUTF8Texts max = StateT $ 
    \(currentLength, txt) -> do 
        word <- liftIO utf8DictWord
        if T.length word + currentLength < fromInteger max
          then runStateT (buildUTF8Texts max) 
                 (currentLength + T.length word, 
                  txt <> (T.singleton ' ') <> word)
          else return (txt, (currentLength, txt))

buildUnicodeTexts :: Integer -> TextResult
buildUnicodeTexts max = StateT $ 
    \(currentLength, txt) -> do
        word <- liftIO unicodeDictWord
        if T.length word + currentLength < fromInteger max
          then runStateT (buildUnicodeTexts max) 
                   (currentLength + T.length word,
                    txt <> (T.singleton ' ') <> word)
          else return (txt, (currentLength, txt))

name :: Size -> IO T.Text
name Max          = getName >>= \x -> return $ T.take 8000 x
name (Size _ max) = getName >>= \x -> return $ T.take (fromIntegral max) x

-- | Text Building Helpers

utf8Dict :: IO (V.Vector T.Text)
utf8Dict = do
  -- Read file in and set encoding to UTF8
  h <- openFile "data/dict.txt" ReadMode
  hSetEncoding h latin1
  wl <- TI.hGetContents h
  return (V.fromList $ T.lines wl)

problemStrings :: IO (V.Vector T.Text)
problemStrings = do
  h <- openFile "data/problems.txt" ReadMode
  hSetEncoding h latin1
  wl <- TI.hGetContents h
  return (V.fromList $ T.lines wl)

unicodeDict :: IO (V.Vector T.Text)
unicodeDict = do
  wl <- TI.readFile "data/greek.txt"
  return (V.fromList $ T.lines wl)

names :: IO (V.Vector T.Text)
names = do
  nl <- TI.readFile "data/names.txt"
  return (V.fromList $ T.lines nl)

getName :: IO T.Text
getName = names >>= getWord

utf8DictWord :: IO T.Text
utf8DictWord = utf8Dict >>= getWord

problemString :: IO T.Text
problemString = problemStrings >>= getWord

unicodeDictWord :: IO T.Text
unicodeDictWord = unicodeDict >>= getWord

getWord :: V.Vector T.Text -> IO T.Text
getWord wl = do 
  randomIndex <- randomRIO (0, V.length wl - 1)
  let removeApostrophe = T.replace (T.singleton '\'') T.empty
  return $ removeApostrophe $ wl V.! randomIndex

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
