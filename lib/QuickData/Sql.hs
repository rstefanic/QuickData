{-# LANGUAGE OverloadedStrings #-}

module QuickData.Sql 
    (insertValues
    ) where

import Control.Monad.State.Lazy       (evalStateT)
import Control.Monad.Trans.State.Lazy
import Data.DateTime 
import Data.Functor.Identity
import Data.List                as L
import Data.Text
import Data.Text                as T
import Prelude                  hiding (min, max)
import System.Random            (randomRIO)

import QuickData.Internal
import qualified QuickData.Randomize as Randomize

type ColumnStates a = StateT [Column] Identity a

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
createValuesFromTable t = traverse id $ createValues cols number
    where number = numberOfRecords $ metaData t
          cols = columns t
          createValues c n =
              let (values, c') = runIdentity $ runStateT getValuesForColumn c
              in if n > 1
                  then values : createValues c' (n - 1)
                  else [values]

getValuesForColumn :: ColumnStates (IO Text)
getValuesForColumn = do
    s <- get
    let values = genColValue <$> s
    let values' = (wrapInsertInParentheses . fmap (T.intercalate ", ") . sequence) values
    let updatedCols = updateCols s
    put updatedCols
    return values'

updateCols :: [Column] -> [Column]
updateCols []     = []
updateCols (x:xs) 
    | (SqlPK t n) <- columnType x = (x { columnType = SqlPK t (n + 1) }) : updateCols xs
    | otherwise                   = x : updateCols xs

genColValue :: Column -> IO Text
genColValue c 
    | (SqlPK _ i) <- columnType c  = return $ (pack . show) i
    | allowNull c = do
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
getRandomizedTypeData (SqlBigInt   r)       = pack . show <$> Randomize.randomizeFromRange r
getRandomizedTypeData (SqlInt      r)       = pack . show <$> Randomize.randomizeFromRange r
getRandomizedTypeData (SqlSmallInt r)       = pack . show <$> Randomize.randomizeFromRange r
getRandomizedTypeData (SqlTinyInt  r)       = pack . show <$> Randomize.randomizeFromRange r
getRandomizedTypeData SqlBit                = pack . show <$> Randomize.bit
getRandomizedTypeData SqlFloat              = pack . show <$> Randomize.float
getRandomizedTypeData SqlDateTime           = pack . toSqlString <$> Randomize.dateTime
getRandomizedTypeData SqlDate               = pack . formatDateTime "yyyy-MM-dd" <$> Randomize.dateTime
getRandomizedTypeData (SqlBinary size)      = Randomize.bigInt >>= \int -> return . pack $ castToBinary size int
getRandomizedTypeData (SqlChar size tv)     = getRandomizedTypeData (SqlVarChar size tv)
getRandomizedTypeData (SqlNChar size tv)    = getRandomizedTypeData (SqlNVarChar size tv)
getRandomizedTypeData (SqlText size tv)     = getRandomizedTypeData (SqlVarChar size tv)
getRandomizedTypeData (SqlNText size tv)    = getRandomizedTypeData (SqlNVarChar size tv)
getRandomizedTypeData (SqlVarBinary size)   = buildTexts Randomize.buildUTF8Texts size >>= 
                                                \value -> return . pack $ castToVarBinary size value
getRandomizedTypeData (SqlVarChar size tv)  = case tv of
                                                Just Name -> pack <$> Randomize.name size
                                                _         -> buildTexts Randomize.buildUTF8Texts size
getRandomizedTypeData (SqlNVarChar size tv) = case tv of
                                                Just Name -> pack <$> Randomize.name size
                                                _         -> buildTexts Randomize.buildUnicodeTexts size
getRandomizedTypeData _                     = error $ "Could not determine type"

castToBinary :: Size -> Integer -> String
castToBinary Max value          = castToBinary (Size 0 8000) value
castToBinary (Size _ max) value = "CAST( " ++ show value ++ " AS BINARY(" ++
                                show max ++ ") )"

castToVarBinary :: Size -> Text -> String
castToVarBinary Max value          = castToVarBinary (Size 0 8000) value
castToVarBinary (Size _ max) value = "CAST( '" ++ unpack value ++ "' AS VARBINARY(" ++
                                       show max ++ ") )"

buildTexts :: (Integer -> Randomize.TextResult) -> Size -> IO Text
buildTexts textResult Max = buildTexts textResult $ Size 0 8000
buildTexts textResult (Size minRange maxRange) = do
  randomMin <- randomRIO (minRange, maxRange)
  wrapInSingleQuotes $ pack . L.unwords
    <$> evalStateT (textResult maxRange) (fromIntegral randomMin, [])
