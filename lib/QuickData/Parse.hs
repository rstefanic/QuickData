{-# LANGUAGE OverloadedStrings #-}

module QuickData.Parse 
        ( getConfig
        , tableInfoFile
        ) where

import QuickData.Internal 

import           Data.Aeson
import qualified Data.ByteString.Lazy      as B
import qualified Data.HashMap              as HM
import qualified Data.Text                 as T
import           Control.Monad
import           Control.Monad.Trans.Maybe

instance FromJSON Table where
    parseJSON = withObject "Table" $ \obj -> do
        metaData <- obj .: "metaData"
        columns  <- (obj .: "columns") >>= fmap Columns . parseJSON
        return $ Table metaData columns

instance FromJSON MetaData where
    parseJSON = withObject "MetaData" $ \x -> MetaData
        <$> x .: "tableName"
        <*> x .: "rowCount"

instance FromJSON Column where
    parseJSON = withObject "Column" $ \x -> do
        columnName <- x .: "columnName"
        columnType <- toSqlType <$> x .: "columnType"
        allowNull  <- x .: "allowNull"
        return $ Column columnName columnType allowNull

instance FromJSON Size
instance FromJSON TextValue
instance FromJSON SqlType

toSqlType :: T.Text -> SqlType
toSqlType "SqlVarChar" = SqlVarChar (Size 80) (Just DictWords)
toSqlType _            = SqlInt

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