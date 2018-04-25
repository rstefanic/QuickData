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
    ) where

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
    = Size Integer
    | Max
    deriving (Eq, Show, Generic)

data TextValue 
    = DictWords
    | Name 
    deriving (Eq, Show, Generic)

data SqlType 
    = SqlBigInt
    | SqlInt 
    | SqlSmallInt
    | SqlTinyInt
    | SqlBit 
    | SqlFloat
    | SqlDate
    | SqlDateTime
    | SqlText
    | SqlChar      Size TextValue
    | SqlVarChar   Size TextValue
    | SqlBinary    Size TextValue
    | SqlVarBinary Size TextValue
    deriving (Eq, Show, Generic)