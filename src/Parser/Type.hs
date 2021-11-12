{-# LANGUAGE OverloadedStrings #-}

module Parser.Type where

import Model.Type
import Text.Megaparsec.Char
import Text.Megaparsec
import Parser.General 
  
typeParser :: Parser Type
typeParser = 
    do 
        tName <- try typeNameParser
        tSuper <- optional superTypeParser
        _ <- lexeme $ char ':'
        tDescription <- optional descriptionParser
        tAttributes <- many $ try typeAttributeParser
        return (MakeType tName tSuper tDescription tAttributes)

superTypeParser :: Parser Type
superTypeParser =
    do
        _ <- lexeme $ string "extends"
        name <- pascalNameParser 
        return $ MakeType name Nothing Nothing []

typeAttributeParser :: Parser TypeAttribute
typeAttributeParser = 
    do
        aName <- try camelNameParser
        aType <- try nameParser
        card <- cardinalityParser
        desc <- optional descriptionParser
        return (MakeTypeAttribute aName (MakeType aType Nothing Nothing []) card desc)

cardinalityParser :: Parser Cardinality
cardinalityParser =
    do
        try parseBounded <|> try parseSemiBounded <|> try parseUnbounded

parseBounded :: Parser Cardinality
parseBounded = 
    do
        _ <- lexeme $ char '('
        low <- lexeme $ many digitChar
        _ <- lexeme $ string ".."
        up <- lexeme $ many digitChar
        _ <- lexeme $ char ')'
        return $ Bounds (read low, read up)


parseSemiBounded :: Parser Cardinality
parseSemiBounded = 
    do
        _ <- lexeme $ char '('
        low <- lexeme $ many digitChar
        _ <- lexeme $ string "..*)"
        return $ OneBound $ read low
        
        
parseUnbounded :: Parser Cardinality
parseUnbounded = 
    do
        _ <- lexeme $ string "(*..*)"
        return NoBounds

typeNameParser :: Parser String
typeNameParser = 
    do
        _ <- lexeme $ string "type"
        pascalNameParser