{-# LANGUAGE DeriveGeneric #-}

module QuickData.Types where

import Dhall
import Data.DateTime
import Data.Text      as T

type Columns = [Column]
newtype Words = Words [String] deriving (Eq, Show)

data Column = 
     Column { columnName :: T.Text
            , columnType :: T.Text
            , allowNull  :: Bool
            }
     deriving (Eq, Show)

data MetaData =
     MetaData { tableName :: !T.Text
              , rowCount  :: !Int 
              }
     deriving (Eq, Show)

data Table = 
     Table { metaData :: MetaData
           , columns  :: Columns       
           }
     deriving (Eq, Show)

data Size 
    = Size Int
    | Max
    deriving (Eq, Show)

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
    | SqlNText 
    | SqlChar Size
    | SqlNChar Size
    | SqlVarChar Size
    | SqlNVarChar Size
    | SqlBinary Size
    | SqlVarBinary Size
    deriving (Eq, Show)

data Example = 
     Example { foo :: Integer
             , bar :: Vector Double 
             }
  deriving (Generic, Show)

instance Interpret Example