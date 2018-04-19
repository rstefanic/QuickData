{-# LANGUAGE OverloadedStrings #-}

module QuickData.Generate where

import QuickData.Types
import QuickData.Randomize

import Control.Monad.State.Lazy
import qualified Data.Text as T

withinLength :: String -> Int -> Bool
withinLength str i | length str < i = True
                   | otherwise      = False

cutoffLength :: String -> Int -> String
cutoffLength str max = cutoffLength' str 0
  where cutoffLength' []     n = []
        cutoffLength' (x:xs) n | n < max   = x : cutoffLength' xs (n + 1)
                               | otherwise = [x]

getName :: Size -> IO String
getName Max      = randomName >>= \name -> return $ cutoffLength name 8000
getName (Size n) = randomName >>= \name -> return $ cutoffLength name n

buildLongTexts :: Int -> StateT (Int, [String]) IO [String]
buildLongTexts max = StateT $ 
    \(currentLength, str) -> do 
        word <- liftIO randomDictWord
        if length word + currentLength < max
        then runStateT (buildLongTexts max) (currentLength + length word, str ++ [word])
        else return (str, (currentLength, str))