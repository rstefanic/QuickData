{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickData.Internal 
    ( Table(..)
    , Column(..)
    , MetaData(..)
    , Size(..)
    , TextValue(..)
    , SqlType(..)
    , intRange
    , bigIntRange
    , tinyIntRange
    , smallIntRange
    , checkRange
    ) where

import qualified Data.Text as T
import           Prelude        hiding (min, max)

-- | AST for Table

data Table = Table 
    { metaData :: !MetaData
    , columns  :: ![Column]
    } deriving (Eq, Show)

data MetaData = MetaData
    { tableName       :: !T.Text
    , numberOfRecords :: !Integer
    } deriving (Eq, Show)

data Column = Column 
    { columnName :: !T.Text
    , columnType :: !SqlType
    , allowNull  :: !Bool
    } deriving (Eq, Show)

type Min = Integer
type Max = Integer

data Size 
    = Size Min Max
    | Max
    deriving (Eq, Show)

data TextValue 
    = DictWords
    | Name 
    deriving (Eq, Show)

data SqlType 
    = SqlBigInt    Size
    | SqlInt       Size
    | SqlSmallInt  Size
    | SqlTinyInt   Size
    | SqlBinary    Size 
    | SqlVarBinary Size
    | SqlText      Size (Maybe TextValue)
    | SqlNText     Size (Maybe TextValue)
    | SqlChar      Size (Maybe TextValue)
    | SqlNChar     Size (Maybe TextValue)
    | SqlVarChar   Size (Maybe TextValue)
    | SqlNVarChar  Size (Maybe TextValue)
    | SqlBit 
    | SqlFloat
    | SqlDate
    | SqlDateTime
    deriving (Eq, Show)

-- | Range Constants and helper functions

-- These are the ranges for a given number types in SQLServer
bigIntRange, intRange, tinyIntRange, smallIntRange :: (Integer, Integer)
bigIntRange   = (-9223372036854775808, 9223372036854775807)
intRange      = (-2147483648, 2147483647)
tinyIntRange  = (0, 255)
smallIntRange = (-32768, 32767)

checkRange :: Ord a => (a, a) -> (a, a) -> Bool
checkRange (givenMin, givenMax) (min', max') 
    | givenMin >= min' && givenMax <= max' = True
    | otherwise                            = False
