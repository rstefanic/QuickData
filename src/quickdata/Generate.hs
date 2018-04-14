{-# LANGUAGE OverloadedStrings #-}

module QuickData.Generate where

import QuickData.Types

import qualified Data.Text as T

withinLength :: String -> Int -> Bool
withinLength str i | length str < i = True
                   | otherwise      = False

cutoffLength :: String -> Int -> T.Text
cutoffLength str max = T.concat $ cutoffLength' str 0
  where cutoffLength' []     n = []
        cutoffLength' (x:xs) n | n < max   = T.singleton x : cutoffLength' xs (n + 1)
                               | otherwise = [T.singleton x]
