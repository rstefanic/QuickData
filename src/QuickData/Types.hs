{-# LANGUAGE DeriveGeneric #-}

module QuickData.Types where

import Dhall
import Data.DateTime
import Data.Text      as T

type Columns = [Column]
newtype Words = Words [String] deriving (Eq, Show)

data Column = 
     Column { columnName :: T.Text
            , columnType :: SqlType
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

data TextValue 
    = DictWords
    | Name 
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
    | SqlChar Size (Maybe TextValue)
    | SqlVarChar Size (Maybe TextValue)
    | SqlBinary Size (Maybe TextValue)
    | SqlVarBinary Size (Maybe TextValue)
    -- | SqlNText 
    -- | SqlNChar Size
    -- | SqlNVarChar Size
    deriving (Eq, Show)

data Example = 
     Example { foo :: Integer
             , bar :: Vector Double 
             }
  deriving (Generic, Show)

instance Interpret Example