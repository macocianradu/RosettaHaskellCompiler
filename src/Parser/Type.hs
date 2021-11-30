{-# LANGUAGE OverloadedStrings #-}

module Parser.Type where

import Model.Type
import Text.Megaparsec.Char
import Text.Megaparsec
import Parser.General 
  
-- |Parses a type declaration statement in Rosetta into an Type
typeParser :: Parser Type
typeParser = 
    do 
        tName <- try typeNameParser
        tSuper <- optional superTypeParser
        _ <- lexeme $ char ':'
        tDescription <- optional descriptionParser
        tAttributes <- many $ try typeAttributeParser
        return (MakeType tName tSuper tDescription tAttributes)

-- |Parses the super class declaration statement in Rosetta into an Type
superTypeParser :: Parser Type
superTypeParser =
    do
        _ <- lexeme $ string "extends"
        name <- pascalNameParser 
        return $ MakeType name Nothing Nothing []

-- |Parses a declared type attribute in Rosetta into a TypeAttribute
typeAttributeParser :: Parser TypeAttribute
typeAttributeParser = 
    do
        aName <- try camelNameParser
        aType <- try nameParser
        card <- cardinalityParser
        desc <- optional descriptionParser
        return (MakeTypeAttribute aName (MakeType aType Nothing Nothing []) card desc)

-- |Parses the cardinality of a type attribute in Rosetta into a Cardinality
cardinalityParser :: Parser Cardinality
cardinalityParser =
    do
        try parseBounded <|> try parseSemiBounded <|> try parseUnbounded

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
        
-- |Parses an unbounded cardinality statement in Rosetta into a Cardinality
parseUnbounded :: Parser Cardinality
parseUnbounded = 
    do
        _ <- lexeme $ string "(*..*)"
        return NoBounds

-- |Parses the name of a type in Rosetta into a String
typeNameParser :: Parser String
typeNameParser = 
    do
        _ <- lexeme $ string "type"
        pascalNameParser