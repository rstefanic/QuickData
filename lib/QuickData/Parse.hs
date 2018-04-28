{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module QuickData.Parse 
    ( getConfig
    ) where

import           Data.Aeson
import qualified Data.Aeson.Types          as AT
import qualified Data.ByteString.Lazy      as B
import qualified Data.Text                 as T
import           Control.Monad.Trans.Maybe
import           GHC.Generics              (Generic)

import QuickData.Internal 

instance FromJSON Table where
    parseJSON = withObject "Table" $ \obj -> do
        metaData <- obj .: "metaData"
        columns  <- (obj .: "columns") >>= fmap Columns . parseJSON
        return $ Table metaData columns

instance FromJSON MetaData where
    parseJSON = withObject "MetaData" $ \x -> MetaData
        <$> x .: "tableName"
        <*> x .: "rowCount"

data ValueInfo = ValueInfo { size      :: Integer
                           , textValue :: Maybe TextValue }
    deriving (Eq, Show, Generic)
instance FromJSON ValueInfo 

instance FromJSON Column where
    parseJSON = withObject "Column" $ \x -> do
        columnName <- x .: "columnName"
        columnType <- x .: "columnType"
        textInfo   <- x .:? "textInfo" :: AT.Parser (Maybe ValueInfo)
        allowNull  <- x .: "allowNull"
        return $ Column columnName (toSqlType textInfo columnType) allowNull

instance FromJSON Size
instance FromJSON TextValue
instance FromJSON SqlType

toSqlType :: Maybe ValueInfo -> T.Text -> SqlType
toSqlType _ "BigInt"            = SqlBigInt
toSqlType _ "Int"               = SqlInt
toSqlType _ "SmallInt"          = SqlSmallInt
toSqlType _ "TinyInt"           = SqlTinyInt
toSqlType _ "Bit"               = SqlBit
toSqlType _ "Float"             = SqlFloat
toSqlType _ "Date"              = SqlDate
toSqlType _ "DateTime"          = SqlDateTime
toSqlType _ "Text"              = SqlText
toSqlType (Just vi) "Char"      = SqlChar      (Size $ size vi) (textValue vi)
toSqlType (Just vi) "VarChar"   = SqlVarChar   (Size $ size vi) (textValue vi)
toSqlType Nothing   "Char"      = SqlChar      (Size 80)        (Just DictWords)
toSqlType Nothing   "VarChar"   = SqlChar      (Size 80)        (Just DictWords)
toSqlType (Just vi) "Binary"    = SqlBinary    (Size $ size vi) Nothing
toSqlType (Just vi) "VarBinary" = SqlVarBinary (Size $ size vi) (textValue vi)
toSqlType Nothing   "Binary"    = SqlBinary    (Size 10)        Nothing
toSqlType Nothing   "VarBinary" = SqlVarBinary (Size 50)        (Just DictWords)
toSqlType _ errType             = error $ T.unpack $ T.concat 
                                    ["Could not parse ", errType, " into a SQL Type."]

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