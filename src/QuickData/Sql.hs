{-# LANGUAGE OverloadedStrings #-}

module QuickData.Sql where

import QuickData.Randomize
import QuickData.Types
import Control.Monad

import Data.List as L
import Data.Text as T

insertValues :: Table -> IO Text
insertValues table = do
    values <- createValuesFromTable table
    let columnNames = columnName <$> columns table
    return $ T.concat [ "INSERT INTO "
                      , tableName $ metaData table 
                      , T.concat [" (", T.intercalate ", " columnNames, ") "]
                      , "VALUES "
                      , T.intercalate ", " values
                      , ";"
                      ] 

createValuesFromTable :: Table -> IO [Text]
createValuesFromTable table = traverse id $ createValues (columns table) (rowCount $ metaData table)
    where createValues columns n 
                | n > 1 = getValuesForColumn columns : createValues columns (n - 1)
                | otherwise = [getValuesForColumn columns]

getValuesForColumn :: [Column] -> IO Text
getValuesForColumn = wrapInsertInParentheses 
                     . fmap (T.intercalate ", ") 
                     . sequence 
                     . fmap (getRandomizedTypeData . columnType)

wrapInsertInParentheses :: IO Text -> IO Text
wrapInsertInParentheses text = text >>= \text' -> return $ T.concat ["(", text', ")"]

getRandomizedTypeData :: SqlType -> IO Text
getRandomizedTypeData sqlType = 
    case sqlType of
        SqlBigInt   -> pack . show <$> randomBigInt
        SqlInt      -> pack . show <$> randomInt
        SqlSmallInt -> pack . show <$> randomSmallInt
        SqlTinyInt  -> pack . show <$> randomTinyInt
        SqlBit      -> pack . show <$> randomBit
        SqlFloat    -> pack . show <$> randomFloat
        SqlDateTime -> pack . show <$> randomDateTime
        --SqlDate   -> pack . show <$> randomDate
        --SqlChar _ -> pack . show <$> randomChar
        --SqlText   -> pack . show <$> randomText
        _           -> return $ pack ("ERR" :: String)