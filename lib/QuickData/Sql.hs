{-# LANGUAGE OverloadedStrings #-}

module QuickData.Sql 
    ( insertValues
    ) where

import Control.Monad            (when)
import Control.Monad.State.Lazy (evalStateT)
import Control.Monad.Trans.State.Lazy
import Data.DateTime 
import Data.Functor.Identity
import Data.Text                as T
import Prelude                  hiding (min, max)
import System.Random            (randomRIO)

import QuickData.Internal
import qualified QuickData.Randomize as Randomize

type ColumnStates a = StateT [Column] Identity a

insertValues :: Table -> IO T.Text
insertValues table = do
    when (not $ checkPKRanges table) $ do 
        error $ "An initial range given for a PK will exceed the upper " ++
                "bounds of that type." 
    values <- createValuesFromTable table
    let columnNames = columnName <$> columns table
    return $ T.concat [ "INSERT INTO "
                      , tableName $ metaData table 
                      , T.concat [" (", T.intercalate ", " columnNames, ") "]
                      , "VALUES "
                      , T.intercalate ", " values
                      , ";"
                      ] 

checkPKRanges :: Table -> Bool
checkPKRanges t = Prelude.all (== True) $ go c 
    where n  = numberOfRecords $ metaData t
          c  = columnType <$> columns t
          go []                         = [True]
          go (x:xs) | (SqlPK t' i) <- x = typeWithinRange t' i n : go xs
                    | otherwise         = [True] ++ go xs

typeWithinRange :: SqlType -> Integer -> Integer -> Bool
typeWithinRange (SqlBigInt _)   i n = checkRange (i, i + n) bigIntRange
typeWithinRange (SqlInt _)      i n = checkRange (i, i + n) intRange
typeWithinRange (SqlTinyInt _)  i n = checkRange (i, i + n) tinyIntRange
typeWithinRange (SqlSmallInt _) i n = checkRange (i, i + n) smallIntRange
typeWithinRange _ _ _               = True

createValuesFromTable :: Table -> IO [T.Text]
createValuesFromTable t = traverse id $ createValues cols number
    where number = numberOfRecords $ metaData t
          cols = columns t
          createValues c n =
              let (values, c') = runIdentity $ runStateT getValuesForColumn c
              in if n > 1
                  then values : createValues c' (n - 1)
                  else [values]

getValuesForColumn :: ColumnStates (IO T.Text)
getValuesForColumn = do
    s <- get
    let values = genColValue <$> s
    let values' = (fmap wrapInsertInParentheses .
                   fmap (T.intercalate ", ") . 
                   sequence) values
    let updatedCols = updateCols s
    put updatedCols
    return values'

updateCols :: [Column] -> [Column]
updateCols []     = []
updateCols (x:xs) 
    | (SqlPK t n) <- columnType x = 
            (x { columnType = SqlPK t (n + 1) }) : updateCols xs

    | otherwise = x : updateCols xs

genColValue :: Column -> IO T.Text
genColValue c 
    | (SqlPK _ i) <- columnType c  = return $ (T.pack . show) i
    | allowNull c = do
        r <- randomRIO (0, 1) :: IO Int
        if r == 1
            then return $ T.pack "NULL"
            else getDataFromType c
    | otherwise   = getDataFromType c
    where getDataFromType = getRandomizedTypeData . columnType

wrapInsertInParentheses :: T.Text -> T.Text
wrapInsertInParentheses txt = T.concat ["(", txt, ")"]

wrapInSingleQuotes :: T.Text -> T.Text
wrapInSingleQuotes txt = T.concat ["'", txt, "'"]

getRandomizedTypeData :: SqlType -> IO T.Text 
getRandomizedTypeData (SqlBigInt   r)       = T.pack . show <$> Randomize.randomizeFromRange r
getRandomizedTypeData (SqlInt      r)       = T.pack . show <$> Randomize.randomizeFromRange r
getRandomizedTypeData (SqlSmallInt r)       = T.pack . show <$> Randomize.randomizeFromRange r
getRandomizedTypeData (SqlTinyInt  r)       = T.pack . show <$> Randomize.randomizeFromRange r
getRandomizedTypeData SqlBit                = T.pack . show <$> Randomize.bit
getRandomizedTypeData SqlFloat              = T.pack . show <$> Randomize.float
getRandomizedTypeData SqlDateTime           = T.pack . toSqlString <$> Randomize.dateTime
getRandomizedTypeData SqlDate               = T.pack . formatDateTime "yyyy-MM-dd" <$> Randomize.dateTime
getRandomizedTypeData (SqlBinary size)      = Randomize.bigInt >>= \int -> return $ castToBinary size int
getRandomizedTypeData (SqlChar size tv)     = getRandomizedTypeData (SqlVarChar size tv)
getRandomizedTypeData (SqlNChar size tv)    = getRandomizedTypeData (SqlNVarChar size tv)
getRandomizedTypeData (SqlText size tv)     = getRandomizedTypeData (SqlVarChar size tv)
getRandomizedTypeData (SqlNText size tv)    = getRandomizedTypeData (SqlNVarChar size tv)

getRandomizedTypeData (SqlVarBinary size)   = 
    buildTexts Randomize.buildUTF8Texts size >>= 
        \value -> return $ castToVarBinary size value

getRandomizedTypeData (SqlVarChar size tv)  = case tv of
    Just Name          -> Randomize.name size
    Just ProblemString -> Randomize.problemString
    _                  -> buildTexts Randomize.buildUTF8Texts size

getRandomizedTypeData (SqlNVarChar size tv) = case tv of
    Just Name          -> Randomize.name size
    Just ProblemString -> Randomize.problemString
    _                  -> buildTexts Randomize.buildUnicodeTexts size

getRandomizedTypeData _                     = error $ "Could not determine type"

castToBinary :: Size -> Integer -> T.Text
castToBinary Max value          = castToBinary (Size 0 8000) value
castToBinary (Size _ max) value = T.concat ["CAST( ", (T.pack . show) value
                                           , " AS BINARY(" 
                                           , (T.pack . show) max, ") )"]

castToVarBinary :: Size -> T.Text -> T.Text
castToVarBinary Max value          = castToVarBinary (Size 0 8000) value
castToVarBinary (Size _ max) value = T.concat ["CAST( '", value
                                              , "' AS VARBINARY("
                                              , (T.pack . show) max
                                              , ") )"]

buildTexts :: (Integer -> Randomize.TextResult) -> Size -> IO T.Text
buildTexts textResult Max = buildTexts textResult $ Size 0 8000
buildTexts textResult (Size minRange maxRange) = do
    randomMin <- randomRIO (minRange, maxRange)
    wrapInSingleQuotes <$>
      evalStateT (textResult maxRange) (fromIntegral randomMin, T.empty)
