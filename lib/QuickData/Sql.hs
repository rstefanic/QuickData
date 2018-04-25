{-# LANGUAGE OverloadedStrings #-}

module QuickData.Sql 
    ( insertValues
    ) where

import           QuickData.Internal
import qualified QuickData.Randomize as Randomize

import Control.Monad.State.Lazy
import Data.DateTime 
import Data.List                as L
import Data.Text                as T

insertValues :: Table -> IO Text
insertValues table = do
    values <- createValuesFromTable table
    let columnNames = columnName <$> (getColumns . columns) table
    return $ T.concat [ "INSERT INTO "
                      , tableName $ metaData table 
                      , T.concat [" (", T.intercalate ", " columnNames, ") "]
                      , "VALUES "
                      , T.intercalate ", " values
                      , ";"
                      ] 

createValuesFromTable :: Table -> IO [Text]
createValuesFromTable table = traverse id $ createValues (getColumns $ columns table) (rowCount $ metaData table)
    where createValues columns' n 
                | n > 1 = getValuesForColumn columns' : createValues columns' (n - 1)
                | otherwise = [getValuesForColumn columns']

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
getRandomizedTypeData SqlBigInt   = pack . show <$> Randomize.bigInt
getRandomizedTypeData SqlInt      = pack . show <$> Randomize.int
getRandomizedTypeData SqlSmallInt = pack . show <$> Randomize.smallInt
getRandomizedTypeData SqlTinyInt  = pack . show <$> Randomize.tinyInt
getRandomizedTypeData SqlBit      = pack . show <$> Randomize.bit
getRandomizedTypeData SqlFloat    = pack . show <$> Randomize.float
getRandomizedTypeData SqlDateTime = pack . toSqlString <$> Randomize.dateTime
getRandomizedTypeData SqlDate     = pack . formatDateTime "yyyy-MM-dd" <$> Randomize.dateTime

-- Text Values
getRandomizedTypeData (SqlVarChar size textValue) = 
    getRandomizedTypeData (SqlChar size textValue)
getRandomizedTypeData (SqlChar size textValue) = 
    case textValue of
        Name -> pack <$> Randomize.name size
        _    -> buildTexts size
getRandomizedTypeData _           = return $ pack ("ERR" :: String)

buildTexts :: Size -> IO Text
buildTexts (Size n) = wrapInSingleQuotes $ pack . L.unwords 
                    <$> evalStateT (Randomize.buildTexts n) (0, [])
buildTexts Max      = buildTexts $ Size 8000