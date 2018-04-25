{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module QuickData.Parse 
        ( getConfig
        ) where

import QuickData.Internal 

import           Data.Aeson
import           Data.Aeson.Types          as AT
import qualified Data.ByteString.Lazy      as B
import qualified Data.HashMap              as HM
import qualified Data.Text                 as T
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           GHC.Generics              (Generic)

instance FromJSON Table where
    parseJSON = withObject "Table" $ \obj -> do
        metaData <- obj .: "metaData"
        columns  <- (obj .: "columns") >>= fmap Columns . parseJSON
        return $ Table metaData columns

instance FromJSON MetaData where
    parseJSON = withObject "MetaData" $ \x -> MetaData
        <$> x .: "tableName"
        <*> x .: "rowCount"

data TextInfo = TextInfo { size      :: Integer
                         , textValue :: TextValue }
    deriving (Eq, Show, Generic)

instance FromJSON TextInfo 

instance FromJSON Column where
    parseJSON = withObject "Column" $ \x -> do
        columnName <- x .: "columnName"
        columnType <- x .: "columnType"
        textInfo   <- x .:? "textInfo" :: AT.Parser (Maybe TextInfo)
        allowNull  <- x .: "allowNull"
        return $ 
            Column columnName (toSqlType textInfo columnType) allowNull

instance FromJSON Size
instance FromJSON TextValue
instance FromJSON SqlType

toSqlType :: Maybe TextInfo -> T.Text -> SqlType
toSqlType _ "BigInt"            = SqlBigInt
toSqlType _ "Int"               = SqlInt
toSqlType _ "SmallInt"          = SqlSmallInt
toSqlType _ "TinyInt"           = SqlTinyInt
toSqlType _ "Bit"               = SqlBit
toSqlType _ "Float"             = SqlFloat
toSqlType _ "Date"              = SqlDate
toSqlType _ "DateTime"          = SqlDateTime
toSqlType _ "Text"              = SqlText
toSqlType (Just ti) "Char"      = SqlChar      (Size $ size ti) (textValue ti)
toSqlType (Just ti) "VarChar"   = SqlVarChar   (Size $ size ti) (textValue ti)
toSqlType (Just ti) "Binary"    = SqlBinary    (Size $ size ti) (textValue ti)
toSqlType (Just ti) "VarBinary" = SqlVarBinary (Size $ size ti) (textValue ti)
toSqlType Nothing "Char"        = SqlChar      Max              DictWords
toSqlType Nothing "VarChar"     = SqlChar      Max              DictWords
toSqlType Nothing "Binary"      = SqlChar      Max              DictWords
toSqlType Nothing "VarBinary"   = SqlChar      Max              DictWords
toSqlType _ _                   = error "Could not parse SqlType"

tableInfoFile :: FilePath
tableInfoFile = "./config.json"

parseConfig :: MaybeT IO Table
parseConfig = MaybeT $ decode <$> B.readFile tableInfoFile 
        
getConfig :: IO Table
getConfig = do 
    table <- runMaybeT parseConfig
    case table of
        Just  x -> return x
        Nothing -> error "Could not parse config file"