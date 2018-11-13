{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module QuickData.Parse 
    ( getConfig
    ) where

import qualified Data.Text                  as T
import           Data.Void
import           Data.Monoid                ((<>))
import           Prelude                    hiding (min, max)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import QuickData.Internal 

type Parser = Parsec Void String

-- | Lexer

spaceConsumer :: Parser ()
spaceConsumer = L.space whitespace (L.skipLineComment "--") empty
    where whitespace = (char ' ' <|> newline) >> return ()

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

integer :: Parser Integer
integer = lexeme L.decimal

rword :: String -> Parser ()
rword word = (lexeme . try) (string' word *> notFollowedBy alphaNumChar)

reservedWords :: [String]
reservedWords = 
    [ "for", "with", "records", "column", "pkcolumn"
    , "name", "nullable", "type", "maxvalue"
    , "textvalue", "min", "max", "true", "false"
    , "bigint", "int", "smallint", "tinyint"
    , "binary", "varbinary", "text", "ntext"
    , "char", "nchar", "varchar", "nvarchar"
    , "bit", "float", "date", "datetime", "dictwords"
    ]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
    where
      p = (:) <$> letterChar <*> many (try (alphaNumChar) 
                <|> (char '_') <|> (char '-'))
      check x = if x `elem` reservedWords
                  then fail $ "keyword " <> show x <> " cannot be used as an id"
                  else return x

doubleTab :: Parser ()
doubleTab = tab >> tab >> return ()

boolean :: Parser Bool
boolean = (True <$ rword "true")
    <|> (False <$ rword "false")

-- | Parser

table :: Parser Table
table = do
    md <- metadata
    col <- many (try column <|> try pkColumn)
    return $ Table md col

metadata :: Parser MetaData
metadata = do
    rword "for"
    tName <- identifier
    rword "with"
    rows <- integer
    rword "records"
    return $ MetaData (T.pack tName) rows

pkColumn :: Parser Column
pkColumn = do
    rword "column:"
    rword "pk"
    _ <- tab
    rword "name:" 
    colName <- identifier
    _ <- tab
    t <- sqlType
    _ <- tab
    rword "from:"
    start <- integer
    return $ Column (T.pack colName) (SqlPK t start) False

column :: Parser Column
column = do
    rword "column:"
    _ <- tab
    rword "name:" 
    colName <- identifier
    _ <- tab
    rword "nullable:"
    nullable <- boolean
    _ <- tab
    rword "type:"
    t <- sqlType
    return $ Column (T.pack colName) t nullable

sqlType :: Parser SqlType
sqlType = typeGen "text" SqlText
    <|> typeGen "ntext" SqlNText
    <|> typeGen "char" SqlChar
    <|> typeGen "nchar" SqlNChar
    <|> typeGen "varchar" SqlVarChar
    <|> typeGen "nvarchar" SqlNVarChar
    <|> sizeOnlyTypeGen "bigint" SqlBigInt
    <|> sizeOnlyTypeGen "int" SqlInt
    <|> sizeOnlyTypeGen "smallint" SqlSmallInt
    <|> sizeOnlyTypeGen "tinyint" SqlTinyInt
    <|> sizeOnlyTypeGen "binary" SqlBinary
    <|> sizeOnlyTypeGen "varbinary" SqlVarBinary
    <|> (SqlBit <$ rword "bit")
    <|> (SqlFloat <$ rword "float")
    <|> (SqlDate <$ rword "date")
    <|> (SqlDateTime <$ rword "datetime")

dictWords :: Parser (Maybe TextValue)
dictWords = do
    doubleTab
    rword "textvalue:"
    rword "dictwords"
    return $ Just DictWords

nameWords :: Parser (Maybe TextValue)
nameWords = do
    doubleTab
    rword "textvalue:"
    rword "name"
    return $ Just Name

max :: Parser Integer
max = do
    doubleTab
    rword "max:"
    i <- integer
    return i

min :: Parser Integer
min = do
    doubleTab
    rword "min:"
    i <- integer
    return i

size :: Parser Size
size = minAndMax >>= \(mn, mx) -> return $ Size mn mx

    where minAndMax = do
            (try (do
                n <- min
                i <- max
                return (n, i)) <?> "Min after Max")
            <|>
            (try (do
                i <- max
                n <- min
                return (n, i)) <?> "Max after Min")
            <|>
            (try (do
                i <- max
                return (0, i)) <?> "Max after Min")
        
textValue :: Parser (Maybe TextValue)
textValue = do
    option Nothing (try dictWords <|> try nameWords)

-- | Parser Generators

typeGen :: String -> (Size -> Maybe TextValue -> SqlType) -> Parser SqlType
typeGen rw t = do
    rword rw
    (size', textVal) <- sizeAndTextVal
    case size' of
        Nothing -> return $ t Max textVal
        Just s  -> return $ t s textVal
    
    where sizeAndTextVal = do
            try (do
                size' <- size
                text  <- textValue
                return (Just size', text))
            <|>
            try (do
                text  <- textValue
                size' <- size
                return (Just size', text))
            <|>
            try (do
                text <- textValue
                return (Nothing, text))

sizeOnlyTypeGen :: String -> (Size -> SqlType) -> Parser SqlType
sizeOnlyTypeGen rw t = do
    rword rw
    s <- size
    let t' = t s
    case isValidRange t' of
        True  -> return $ t'
        False -> fail $ "Error with range for " <> (show t')

-- | Validity Check
    
isValidRange :: SqlType -> Bool
isValidRange (SqlBigInt   (Size mn mx)) = checkRange (mn, mx) bigIntRange
isValidRange (SqlInt      (Size mn mx)) = checkRange (mn, mx) intRange
isValidRange (SqlTinyInt  (Size mn mx)) = checkRange (mn, mx) tinyIntRange
isValidRange (SqlSmallInt (Size mn mx)) = checkRange (mn, mx) smallIntRange
isValidRange _                          = True

-- | Exported Functions

tableInfoFile :: FilePath
tableInfoFile = "./config.qd"

getConfig :: Maybe String -> IO Table
getConfig fileName = do 
    file <- case fileName of
        Just f  -> return f
        Nothing -> return tableInfoFile 
    input <- readFile file
    case parse table file input of
        Right x  -> return x
        Left err -> error $ parseErrorPretty err
