{-# LANGUAGE OverloadedStrings #-}

module QuickData.Sql where

import QuickData.Randomize
import QuickData.Generate
import QuickData.Types

import Control.Monad
import Control.Monad.State.Lazy
import Data.DateTime 
import Data.List     as L
import Data.Text     as T

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

wrapInSingleQuotes :: IO Text -> IO Text
wrapInSingleQuotes text = text >>= \text' -> return $ T.concat ["'", text', "'"]

getRandomizedTypeData :: SqlType -> IO Text

-- Number and Dates
getRandomizedTypeData SqlBigInt   = pack . show <$> randomBigInt
getRandomizedTypeData SqlInt      = pack . show <$> randomInt
getRandomizedTypeData SqlSmallInt = pack . show <$> randomSmallInt
getRandomizedTypeData SqlTinyInt  = pack . show <$> randomTinyInt
getRandomizedTypeData SqlBit      = pack . show <$> randomBit
getRandomizedTypeData SqlFloat    = pack . show <$> randomFloat
getRandomizedTypeData SqlDateTime = pack . toSqlString <$> randomDateTime
getRandomizedTypeData SqlDate     = pack . formatDateTime "yyyy-MM-dd" <$> randomDateTime

-- Text Values
getRandomizedTypeData (SqlVarChar size textValue) = 
    getRandomizedTypeData (SqlChar size textValue)
getRandomizedTypeData (SqlChar size textValue) = 
    case textValue of
        Just Name -> pack <$> getName size
        _         -> buildTexts size
getRandomizedTypeData _           = return $ pack ("ERR" :: String)

buildTexts :: Size -> IO Text
buildTexts (Size n) = wrapInSingleQuotes $ pack . L.unwords 
                    <$> evalStateT (buildLongTexts n) (0, [])
buildTexts Max      = buildTexts $ Size 8000