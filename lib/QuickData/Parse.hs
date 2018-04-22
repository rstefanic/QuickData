{-# LANGUAGE OverloadedStrings #-}

module QuickData.Parse 
        ( getConfig
        ) where

import QuickData.Internal 

import           Data.Aeson
import qualified Data.ByteString.Lazy      as B
import           Control.Monad
import           Control.Monad.Trans.Maybe

instance FromJSON Table where
    parseJSON = withObject "Table" $ \x -> Table
        <$> x .: "metaData"
        <*> x .: "columns"

instance FromJSON MetaData where
    parseJSON = withObject "MetaData" $ \x -> MetaData
        <$> x .: "tableName"
        <*> x .: "rowCount"

instance FromJSON Column where
    parseJSON = withObject "Column" $ \x -> Column
        <$> x .: "coulmnName"
        <*> x .: "columnType"
        <*> x .: "allowNull"

instance FromJSON Size
instance FromJSON TextValue
instance FromJSON SqlType

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