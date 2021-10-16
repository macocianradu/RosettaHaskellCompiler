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
        eDescription <- descriptionParser
        values <- many enumValueParser
        _ <- spaceConsumer
        return (MakeEnum eName (Just eDescription) values)

--parseTest enumValueParser "D displayName \"day\" <\"Day\">"        
enumValueParser :: Parser EnumValue
enumValueParser = 
    do
        vName <- enumValueNameParser
        dName <- enumValueDisplayNameParser
        vDescription <- descriptionParser
        return (MakeEnumValue vName (Just vDescription) (Just dName))

enumValueNameParser :: Parser String
enumValueNameParser = 
    do
        name <- nameParser
        _ <- spaceConsumer
        return name

enumValueDisplayNameParser :: Parser String
enumValueDisplayNameParser =
    do
        _ <- string "displayName \""
        name <- anySingle `manyTill` char '"'
        _ <- spaceConsumer
        return name

enumNameParser :: Parser String
enumNameParser = 
    do
        _ <- string "enum"
        _ <- spaceConsumer
        name <- nameParser
        _ <- char ':'
        _ <- spaceConsumer
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