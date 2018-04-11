{-# LANGUAGE DeriveGeneric #-}

module QuickData.Types where

import Dhall
import Data.DateTime

newtype Words = Words [String] deriving (Eq, Show)

data Example = Example { foo :: Integer, bar :: Vector Double }
  deriving (Generic, Show)

instance Interpret Example

data SqlServerTypes 
    = SSBigInt
    | SSInt 
    | SSSmallInt
    | SSTinyInt
    | SSBit 
    | SSFloat
    | SSDate
    | SSDateTime
    | SSChar
    | SSText
    | SSVarChar
    | SSNChar
    | SSNText
    | SSNVarChar
    | SSBinary
    | SSVarBinary
    deriving (Eq, Show)