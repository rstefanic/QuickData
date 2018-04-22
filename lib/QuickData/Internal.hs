{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module QuickData.Internal where

import           Control.Monad.State.Lazy
import qualified Data.Text                as T
import           GHC.Generics (Generic)

type Columns = [Column]

-- Table Information for Input

data Table = 
     Table { metaData :: MetaData
           , columns  :: Columns       
           }
     deriving (Eq, Show, Generic)

data MetaData =
     MetaData { tableName :: !T.Text
              , rowCount  :: !Integer
              }
     deriving (Eq, Show)

data Column = 
     Column { columnName :: T.Text
            , columnType :: SqlType
            , allowNull  :: Bool
            }
     deriving (Eq, Show)

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
    | SqlChar Size (Maybe TextValue)
    | SqlVarChar Size (Maybe TextValue)
    | SqlBinary Size (Maybe TextValue)
    | SqlVarBinary Size (Maybe TextValue)
    deriving (Eq, Show, Generic)