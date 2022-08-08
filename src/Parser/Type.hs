{-# LANGUAGE OverloadedStrings #-}

module Parser.Type where

import Model.Type
import Text.Megaparsec.Char
import Text.Megaparsec
import Data.Maybe
import Parser.General
import Parser.Expression (expressionParser)

-- |Parses a type declaration statement in Rosetta into an Type
typeParser :: Parser Type
typeParser =
    do
        tName <- try typeNameParser
        tSuper <- superTypeParser
        _ <- lexeme $ char ':'
        tDescription <- optional descriptionParser
        tAttributes <- many $ try typeAttributeParser
        MakeType tName tSuper tDescription tAttributes <$> many ( try conditionParser)

-- |Parses the super class declaration statement in Rosetta into an Type
superTypeParser :: Parser Type
superTypeParser =
    do
        exists <- lexeme $ optional $ string "extends"
        case exists of
            Nothing -> return $ BasicType "Object"
            Just _ -> do
                name <- pascalNameParser
                return $ MakeType name (BasicType "Object") Nothing [] []

-- |Parses a declared type attribute in Rosetta into a TypeAttribute
typeAttributeParser :: Parser TypeAttribute
typeAttributeParser =
    do
        _ <- many $ lexeme metaParser
        aName <- try camelNameParser
        aType <- try nameParser
        card <- cardinalityParser
        desc <- optional descriptionParser
        _ <- many $ lexeme metaParser
        return (MakeTypeAttribute aName (MakeType aType (BasicType "Object") Nothing [] []) card desc)

metaParser :: Parser String
metaParser =
    do
        _ <- lexeme $ char '['
        manyTill (letterChar <|> char ' ') (char ']')

-- |Parses the cardinality of a type attribute in Rosetta into a Cardinality
cardinalityParser :: Parser Cardinality
cardinalityParser = try parseBounded <|> try parseSemiBounded

-- |Parser the condition of a type attribute in Rosetta into a Condition
conditionParser :: Parser Condition
conditionParser = do
    _ <- lexeme $ string "condition"
    _ <- optional $ lexeme pascalNameParser
    _ <- lexeme $ char ':'
    description <- optional descriptionParser
    MakeCondition description <$> expressionParser

-- |Parses a bounded cardinality statement in Rosetta into a Cardinality
parseBounded :: Parser Cardinality
parseBounded =
    do
        _ <- lexeme $ char '('
        low <- lexeme $ many digitChar
        _ <- lexeme $ string ".."
        up <- lexeme $ many digitChar
        _ <- lexeme $ char ')'
        return $ Bounds (read low, read up)

-- |Parses a one bounded cardinality statement in Rosetta into a Cardinality
parseSemiBounded :: Parser Cardinality
parseSemiBounded =
    do
        _ <- lexeme $ char '('
        low <- lexeme $ many digitChar
        _ <- lexeme $ string "..*)"
        return $ OneBound $ read low

-- |Parses the name of a type in Rosetta into a String
typeNameParser :: Parser String
typeNameParser =
    do
        _ <- lexeme $ string "type"
        pascalNameParser