{-# LANGUAGE OverloadedStrings #-}

module QuickData.Sql 
    ( insertValues
    ) where

import Control.Monad.State.Lazy
import Data.DateTime 
import Data.List                as L
import Data.Text                as T
import System.Random            (randomRIO)

import QuickData.Internal
import qualified QuickData.Randomize as Randomize

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
                     . fmap valueOnlyOrNulls

valueOnlyOrNulls :: Column -> IO Text
valueOnlyOrNulls c | allowNull c = do
                        r <- randomRIO (0, 1) :: IO Int
                        if r == 1
                            then return $ pack "NULL"
                            else getDataFromType c
                   | otherwise   = getDataFromType c
    where getDataFromType = getRandomizedTypeData . columnType

wrapInsertInParentheses :: IO Text -> IO Text
wrapInsertInParentheses text = text >>= \text' -> return $ T.concat ["(", text', ")"]

wrapInSingleQuotes :: IO Text -> IO Text
wrapInSingleQuotes text = text >>= \text' -> return $ T.concat ["'", text', "'"]

getRandomizedTypeData :: SqlType -> IO Text
getRandomizedTypeData (SqlBigInt   range) = pack . show <$> Randomize.randomizeFromRange range
getRandomizedTypeData (SqlInt      range) = pack . show <$> Randomize.randomizeFromRange range
getRandomizedTypeData (SqlSmallInt range) = pack . show <$> Randomize.randomizeFromRange range
getRandomizedTypeData (SqlTinyInt  range) = pack . show <$> Randomize.randomizeFromRange range
getRandomizedTypeData SqlBit      = pack . show <$> Randomize.bit
getRandomizedTypeData SqlFloat    = pack . show <$> Randomize.float
getRandomizedTypeData SqlDateTime = pack . toSqlString <$> Randomize.dateTime
getRandomizedTypeData SqlDate     = pack . formatDateTime "yyyy-MM-dd" <$> Randomize.dateTime
getRandomizedTypeData (SqlBinary size) = Randomize.bigInt >>= \int -> return . pack $ castToBinary size int
getRandomizedTypeData (SqlVarBinary size _) = buildTexts size >>= \value -> return . pack $ castToVarBinary size value
getRandomizedTypeData (SqlVarChar size textValue) = getRandomizedTypeData (SqlChar size textValue)
getRandomizedTypeData (SqlChar size textValue) = case textValue of
                                        Just Name -> pack <$> Randomize.name size
                                        _         -> buildTexts size
getRandomizedTypeData _           = return $ pack ("ERR" :: String)

castToBinary :: Size -> Integer -> String
castToBinary Max value          = castToBinary (Size 0 8000) value
castToBinary (Size _ max) value = "CAST( " ++ show value ++ " AS BINARY(" ++
                                show max ++ ") )"

castToVarBinary :: Size -> Text -> String
castToVarBinary Max value          = castToVarBinary (Size 0 8000) value
castToVarBinary (Size _ max) value = "CAST( '" ++ unpack value ++ "' AS VARBINARY(" ++
                                       show max ++ ") )"

buildTexts :: Size -> IO Text
buildTexts Max            = buildTexts $ Size 0 8000
buildTexts (Size min max) = do
                        randomMin <- randomRIO (min, max)
                        wrapInSingleQuotes $ pack . L.unwords 
                          <$> evalStateT (Randomize.buildTexts max) (fromIntegral randomMin, [])
