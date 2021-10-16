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
        tAttributes <- many $ try typeAttributeParserWDesc <|> try typeAttributeParser
        _ <- spaceConsumer
        return (MakeType tName (Just tDescription) tAttributes)

typeAttributeParserWDesc :: Parser TypeAttribute
typeAttributeParserWDesc = 
    do
        (MakeTypeAttribute aName aType card Nothing) <- typeAttributeParser
        descriptionParser >>= \aDescription -> return (MakeTypeAttribute aName aType card (Just aDescription))

typeAttributeParser :: Parser TypeAttribute
typeAttributeParser = 
    do
        aName <- attributeNameParser
        aType <- nameParser
        _ <- spaceConsumer
        card <- cardinalityParser
        _ <- spaceConsumer
        return (MakeTypeAttribute aName aType card Nothing)

cardinalityParser :: Parser Cardinality
cardinalityParser =
    do
        card <- parseExactlyOne <|> parseOneOrMore <|> parseZeroOrMore <|> parseZeroOrOne
        _ <- spaceConsumer
        return card

parseOneOrMore :: Parser Cardinality
parseOneOrMore = 
    do
        _ <- string "(1..*)"
        return OneOrMore


parseExactlyOne :: Parser Cardinality
parseExactlyOne = 
    do
        _ <- string "(1..1)"
        return ExactlyOne
        
        
parseZeroOrMore :: Parser Cardinality
parseZeroOrMore = 
    do
        _ <- string "(0..*)"
        return ZeroOrMore
        
        
parseZeroOrOne :: Parser Cardinality
parseZeroOrOne = 
    do
        _ <- string "(0..1)"
        return ZeroOrOne

attributeNameParser :: Parser String
attributeNameParser = 
    do
        name <- camelNameParser
        _ <- spaceConsumer
        return name

enumValueDisplayNameParser :: Parser String
enumValueDisplayNameParser =
    do
        _ <- string "displayName \""
        name <- anySingle `manyTill` char '"'
        _ <- spaceConsumer
        return name

typeNameParser :: Parser String
typeNameParser = 
    do
        _ <- string "type"
        _ <- spaceConsumer
        name <- pascalNameParser
        _ <- char ':'
        _ <- spaceConsumer
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