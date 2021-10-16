{-# LANGUAGE OverloadedStrings #-}

module Parser.Type where

import Model.Type
import Text.Megaparsec.Char
import Text.Megaparsec
import Parser.General 
  
--parseTest typeParser "type Period: <\"description\"> periodMultiplier int (1..1) <\"A time period multiplier\"> period periodEnum (1..1) <\"A time period \">"
typeParser :: Parser Type
typeParser = 
    do 
        tName <- typeNameParser
        tDescription <- descriptionParser
        tAttributes <- many $ try typeAttributeParser
        return (MakeType tName (Just tDescription) tAttributes)

typeAttributeParser :: Parser TypeAttribute
typeAttributeParser = 
    do
        aName <- camelNameParser
        aType <- nameParser
        card <- cardinalityParser
        desc <- optional descriptionParser
        return (MakeTypeAttribute aName aType card desc)

cardinalityParser :: Parser Cardinality
cardinalityParser =
    do
        parseExactlyOne <|> parseOneOrMore <|> parseZeroOrMore <|> parseZeroOrOne

parseOneOrMore :: Parser Cardinality
parseOneOrMore = 
    do
        _ <- lexeme $ string "(1..*)"
        return OneOrMore


parseExactlyOne :: Parser Cardinality
parseExactlyOne = 
    do
        _ <- lexeme $ string "(1..1)"
        return ExactlyOne
        
        
parseZeroOrMore :: Parser Cardinality
parseZeroOrMore = 
    do
        _ <- lexeme $ string "(0..*)"
        return ZeroOrMore
        
        
parseZeroOrOne :: Parser Cardinality
parseZeroOrOne = 
    do
        _ <- lexeme $ string "(0..1)"
        return ZeroOrOne

typeNameParser :: Parser String
typeNameParser = 
    do
        _ <- lexeme $ string "type"
        name <- pascalNameParser
        _ <- lexeme $ char ':'
        return name 

periodType :: Type
periodType = MakeType 
    "Period" 
    (Just "A class to define recurring periods or time offsets")
    [MakeTypeAttribute 
        "periodMultiplier"
        "Integer"
        ExactlyOne
        (Just "A time period multiplier"),
    
    MakeTypeAttribute 
        "period"
        "periodEnum"
        ExactlyOne
        (Just "A time period")
    ]