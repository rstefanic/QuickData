{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module QuickData.Parse 
    ( getConfig
    ) where

import           Data.Aeson
import qualified Data.Aeson.Types          as AT
import qualified Data.ByteString.Lazy      as B
import qualified Data.Text                 as T
import           Control.Monad.Trans.Either
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

data ValueInfo = ValueInfo { maxValue  :: Integer 
                           , minValue  :: Maybe Integer
                           , textValue :: Maybe TextValue }
    deriving (Eq, Show, Generic)
instance FromJSON ValueInfo 

instance FromJSON Column where
    parseJSON = withObject "Column" $ \x -> do
        columnName <- x .: "columnName"
        columnType <- x .: "columnType"
        textInfo   <- x .:? "valueInfo" :: AT.Parser (Maybe ValueInfo)
        allowNull  <- x .: "allowNull"
        return $ Column columnName (toSqlType textInfo columnType) allowNull

determineSize :: ValueInfo -> Integer -> Size
determineSize valueInfo defaultMin = 
    case minVal of
        Nothing   -> Size defaultMin maxVal
        Just min' -> Size min' maxVal
    where minVal = minValue valueInfo
          maxVal = maxValue valueInfo

sizeFromTuple :: (Integer, Integer) -> Size
sizeFromTuple (x, y) = Size x y

type Range = (Integer, Integer)

determineValidRange :: ValueInfo -> Range -> Bool
determineValidRange v range = 
    case minVal of
        Just m  -> checkRange m maxVal range
        Nothing -> True
    where minVal = minValue v
          maxVal = maxValue v

buildSqlNumType :: ValueInfo -> String -> Range -> (Size -> SqlType) -> SqlType
buildSqlNumType v name range sqlType
    | determineValidRange v range = sqlType (determineSize v (fst range))
    | otherwise                   = outOfRangeErr name

toSqlType :: Maybe ValueInfo -> T.Text -> SqlType
toSqlType (Just vi) "BigInt"    = buildSqlNumType vi "BigInt" bigIntRange SqlBigInt   
toSqlType (Just vi) "Int"       = buildSqlNumType vi "Int" intRange SqlInt
toSqlType (Just vi) "SmallInt"  = buildSqlNumType vi "SmallInt" smallIntRange SqlSmallInt
toSqlType (Just vi) "TinyInt"   = buildSqlNumType vi "TinyInt" tinyIntRange SqlTinyInt
toSqlType (Just vi) "Text"      = SqlText      (determineSize vi 0) (textValue vi)
toSqlType (Just vi) "NText"     = SqlNText     (determineSize vi 0) (textValue vi)
toSqlType (Just vi) "Char"      = SqlChar      (determineSize vi 0) (textValue vi)
toSqlType (Just vi) "NChar"     = SqlNChar     (determineSize vi 0) (textValue vi)
toSqlType (Just vi) "VarChar"   = SqlVarChar   (determineSize vi 0) (textValue vi)
toSqlType (Just vi) "NVarChar"  = SqlNVarChar  (determineSize vi 0) (textValue vi)
toSqlType (Just vi) "VarBinary" = SqlVarBinary (determineSize vi 0)
toSqlType (Just vi) "Binary"    = SqlBinary    (determineSize vi 0) 
toSqlType Nothing   "BigInt"    = SqlBigInt    (sizeFromTuple bigIntRange)
toSqlType Nothing   "Int"       = SqlInt       (sizeFromTuple intRange)
toSqlType Nothing   "SmallInt"  = SqlSmallInt  (sizeFromTuple smallIntRange)
toSqlType Nothing   "TinyInt"   = SqlTinyInt   (sizeFromTuple tinyIntRange)
toSqlType Nothing   "Text"      = SqlText      (sizeFromTuple (0, 80)) (Just DictWords)
toSqlType Nothing   "NText"     = SqlNText     (sizeFromTuple (0, 80)) (Just DictWords)
toSqlType Nothing   "Char"      = SqlChar      (sizeFromTuple (0, 80)) (Just DictWords)
toSqlType Nothing   "NChar"     = SqlNChar     (sizeFromTuple (0, 80)) (Just DictWords)
toSqlType Nothing   "VarChar"   = SqlVarChar   (sizeFromTuple (0, 80)) (Just DictWords)
toSqlType Nothing   "NVarChar"  = SqlNVarChar  (sizeFromTuple (0, 80)) (Just DictWords)
toSqlType Nothing   "VarBinary" = SqlVarBinary (sizeFromTuple (0, 80)) 
toSqlType Nothing   "Binary"    = SqlBinary    (sizeFromTuple (0, 10))  
toSqlType _         "Bit"       = SqlBit
toSqlType _         "Float"     = SqlFloat
toSqlType _         "Date"      = SqlDate
toSqlType _         "DateTime"  = SqlDateTime
toSqlType _         errType     = error $ T.unpack $ T.concat 
                                    ["Could not parse ", errType, " into a SQL Type."]

outOfRangeErr :: String -> SqlType
outOfRangeErr valueType = error $ T.unpack $ T.concat 
                            [ "ERR -- Values given for "
                            , T.pack valueType, " are out of range."]

tableInfoFile :: FilePath
tableInfoFile = "./config.json"

parseConfig :: EitherT String IO Table
parseConfig = EitherT $ eitherDecode <$> B.readFile tableInfoFile 
        
getConfig :: IO Table
getConfig = do 
    table <- runEitherT parseConfig
    case table of
        Right x  -> return x
        Left err -> error $ err ++ "\n\n"