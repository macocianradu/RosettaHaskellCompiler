{-# LANGUAGE OverloadedStrings #-}

module Parser.Enum where

import Parser.General
import Text.Megaparsec.Char
import Text.Megaparsec
import Model.Enum


-- |Parses a complete Rosetta enum into a EnumType
enumParser :: Parser EnumType
enumParser = 
    do 
        eName <- try enumNameParser
        eDescription <- optional descriptionParser
        values <- some enumValueParser
        return (MakeEnum eName eDescription values)
    
    
-- |Parses a Rosetta enum value into a EnumValue
enumValueParser :: Parser EnumValue
enumValueParser = 
    do
        vName <- try nameParser
        dName <- optional enumValueDisplayNameParser
        vDescription <- optional descriptionParser
        return (MakeEnumValue vName vDescription dName)


-- |Parses the display name of a Rosetta enum value into a String
enumValueDisplayNameParser :: Parser String
enumValueDisplayNameParser =
    do
        _ <- lexeme $ string "displayName"
        _ <- char '"'
        lexeme $ anySingle `manyTill` char '"'


-- |Parses the name of a Rosetta enum into a String
enumNameParser :: Parser String
enumNameParser = 
    do
        _ <- lexeme $ string "enum"
        name <- nameParser
        _ <- lexeme $ char ':'
        return name 