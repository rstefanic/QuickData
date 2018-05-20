{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickData.Internal 
    ( Table(..)
    , Column(..)
    , Columns(..)
    , MetaData(..)
    , Size(..)
    , TextValue(..)
    , SqlType(..)
    , getColumns
    , intRange
    , bigIntRange
    , tinyIntRange
    , smallIntRange
    , checkRange
    ) where

import           Data.Aeson   (FromJSON)
import qualified Data.Text                as T
import           GHC.Generics (Generic)

-- Table Information for Input

data Table = 
     Table { metaData :: !MetaData
           , columns  :: !Columns       
           }
     deriving (Eq, Show, Generic)

newtype Columns = Columns [Column]
    deriving (Eq, Show)

getColumns :: Columns -> [Column]
getColumns (Columns x) = x

data MetaData =
     MetaData { tableName :: !T.Text
              , rowCount  :: !Integer
              }
     deriving (Eq, Show, Generic)

data Column = 
     Column { columnName :: !T.Text
            , columnType :: !SqlType
            , allowNull  :: !Bool
            }
     deriving (Eq, Show, Generic)

-- | Sql Types and Helper Types

data Size 
    = Size Integer Integer
    | Max
    deriving (Eq, Show, Generic)
instance FromJSON Size

data TextValue 
    = DictWords
    | Name 
    deriving (Eq, Show, Generic)
instance FromJSON TextValue

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
    deriving (Eq, Show, Generic)
instance FromJSON SqlType

-- | Range Constants and helper functions

bigIntRange, intRange, tinyIntRange, smallIntRange :: (Integer, Integer)
bigIntRange   = (-9223372036854775808, 9223372036854775807)
intRange      = (-2147483648, 2147483647)
tinyIntRange  = (0, 255)
smallIntRange = (-32768, 32767)

checkRange :: Ord a => a -> a -> (a, a) -> Bool
checkRange givenMin givenMax (min', max') 
    | givenMin >= min' && givenMax <= max' = True
    | otherwise                            = False