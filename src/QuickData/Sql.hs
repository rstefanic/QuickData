{-# LANGUAGE OverloadedStrings #-}

module QuickData.Sql where

import QuickData.Randomize
import QuickData.Types
import Control.Monad

import Data.List           as L
import Data.Text           as T

insertValues :: Table -> Text
insertValues table =
    T.concat [ "INSERT INTO "
             , tableName $ metaData table 
             , T.concat [" (", T.intercalate ", " $ fmap columnName (columns table), ") "]
             , "VALUES "
             ] 

-- randomizeValues :: Column -> Text
-- randomizeValues column = getRandomizedTypeData sqlType 
--     where sqlType = columnType column

getRandomizedTypeData :: SqlType -> IO Text
getRandomizedTypeData sqlType = 
    case sqlType of
        SqlBigInt   -> pack . show <$> randomBigInt
        SqlInt      -> pack . show <$> randomInt
        SqlSmallInt -> pack . show <$> randomSmallInt
        SqlTinyInt  -> pack . show <$> randomTinyInt
        SqlBit      -> pack . show <$> randomBit
        SqlFloat    -> pack . show <$> randomFloat
        --SqlDate   -> pack . show <$> randomDate
        SqlDateTime -> pack . show <$> randomDateTime
        --SqlChar _ -> pack . show <$> randomChar
        --SqlText   -> pack . show <$> randomText
        _           -> return $ pack ("ERR" :: String)