{-# LANGUAGE OverloadedStrings #-}

module QuickData.Generate where

import QuickData.Types
import QuickData.Randomize

import Control.Monad.State.Lazy
import qualified Data.Text as T

withinLength :: String -> Int -> Bool
withinLength str i | length str < i = True
                   | otherwise      = False

cutoffLength :: String -> Int -> T.Text
cutoffLength str max = T.concat $ cutoffLength' str 0
  where cutoffLength' []     n = []
        cutoffLength' (x:xs) n | n < max   = T.singleton x : cutoffLength' xs (n + 1)
                               | otherwise = [T.singleton x]

-- getName :: SqlType -> IO T.Text
-- getName sqlType 
--     | SqlText             = randomName
--     | SqlChar    (Size n) = randomName >>= \name -> cutoffLength name n
--     | SqlVarChar (Size n) = randomName >>= \name -> cutoffLength name n
--     | otherwise           = error "Invalid type passed to getName"

buildLongTexts :: Int -> StateT (Int, [String]) IO [String]
buildLongTexts max = StateT $ 
    \(currentLength, str) -> do 
        word <- liftIO randomDictWord
        if length word + currentLength < max
        then runStateT (buildLongTexts max) (currentLength + length word, str ++ [word])
        else return (str, (currentLength, str))