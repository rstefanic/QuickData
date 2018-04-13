{-# LANGUAGE OverloadedStrings #-}

module QuickData.Sql where

import QuickData.Randomize
import QuickData.Types
import Control.Monad

import Data.List as L
import Data.Text as T

insertValues :: Table -> Text
insertValues table =
    T.concat [ "INSERT INTO "
             , tableName $ metaData table 
             , T.concat [" (", T.intercalate ", " $ fmap columnName (columns table), ") "]
             , "VALUES "
             --, createValues table
             ] 

createValues :: Table -> IO [Text]
createValues table = traverse id $ createValues' (columns table) (rowCount $ metaData table)
    where getValues = fmap (getRandomizedTypeData . columnType)
          createValues' :: Columns -> Int -> [IO Text]
          createValues' columns n 
                | n > 1 = getValues columns ++ createValues' columns (n - 1)
                | otherwise = getValues columns

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