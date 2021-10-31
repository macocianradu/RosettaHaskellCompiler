{-# LANGUAGE OverloadedStrings #-}

module Parser.Enum where

import Parser.General
import Text.Megaparsec.Char
import Text.Megaparsec
import Model.Enum

--parseTest enumParser "enum periodEnum: <\"description\"> D displayName \"day\" <\"Day\">"
enumParser :: Parser EnumType
enumParser = 
    do 
        eName <- enumNameParser
        eDescription <- optional descriptionParser
        values <- many enumValueParser
        return (MakeEnum eName eDescription values)

--parseTest enumValueParser "D displayName \"day\" <\"Day\">"        
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

periodEnum :: EnumType
periodEnum = MakeEnum 
    "PeriodEnum" 
    (Just "The enumerated values to specifie the period, e.g. day, week.")
    [MakeEnumValue 
    "D"
    (Just "Day")
    Nothing,
    
    MakeEnumValue
    "W"
    (Just "Week")
    Nothing,
    
    MakeEnumValue
    "Y"
    (Just "Year")
    Nothing
    ]