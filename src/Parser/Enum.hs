{-# LANGUAGE OverloadedStrings #-}

module Parser.Enum where

import Parser.General
import Text.Megaparsec.Char
import Text.Megaparsec
import Model.Enum

enumParser :: Parser EnumType
enumParser = 
    do 
        eName <- enumNameParser
        eDescription <- optional descriptionParser
        values <- some enumValueParser
        return (MakeEnum eName eDescription values)
    
enumValueParser :: Parser EnumValue
enumValueParser = 
    do
        vName <- nameParser
        dName <- optional enumValueDisplayNameParser
        vDescription <- optional descriptionParser
        return (MakeEnumValue vName vDescription dName)

enumValueDisplayNameParser :: Parser String
enumValueDisplayNameParser =
    do
        _ <- lexeme $ string "displayName"
        _ <- char '"'
        lexeme $ anySingle `manyTill` char '"'

enumNameParser :: Parser String
enumNameParser = 
    do
        _ <- lexeme $ string "enum"
        name <- nameParser
        _ <- lexeme $ char ':'
        return name 