{-# LANGUAGE OverloadedStrings #-}

module Parser.Function where

import Parser.Expression
import Parser.Type
import Model.Function
import Model.Type
import Text.Megaparsec
import Text.Megaparsec.Char
import Parser.General
  
functionParser :: Parser Function
functionParser =
    do
        _ <- string "func"
        _ <- spaceConsumer
        fName <- pascalNameParser
        _ <- char ':'
        _ <- spaceConsumer
        fDescription <- descriptionParser
        fInput <- inputAttributesParser
        fOutput <- outputAttributeParser
        fAssignments <- many assignmentParser
        _ <- spaceConsumer
        return (MakeFunction fName (Just fDescription) fInput fOutput fAssignments)

assignmentParser :: Parser (Expression, Expression)
assignmentParser =
    do
        _ <- string "assign-output"
        _ <- spaceConsumer
        name <- expressionParser
        _ <- spaceConsumer
        _ <- char ':'
        _ <- spaceConsumer
        expr <- expressionParser
        _ <- spaceConsumer
        return (name, expr)

inputAttributesParser :: Parser [TypeAttribute]
inputAttributesParser =
    do
        _ <- string "inputs:"
        _ <- spaceConsumer
        inputs <- many $ try attributeParser
        _ <- spaceConsumer
        return inputs

outputAttributeParser :: Parser TypeAttribute
outputAttributeParser =
    do
        _ <- string "output:"
        _ <- spaceConsumer
        outputs <- attributeParser
        _ <- spaceConsumer
        return outputs
        
attributeParser :: Parser TypeAttribute
attributeParser =
    do
        nam <- camelNameParser 
        _ <- spaceConsumer
        typ <- pascalNameParser <|> camelNameParser
        _ <- spaceConsumer
        crd <- cardinalityParser
        _ <- spaceConsumer
        desc <- optional descriptionParser
        return $ MakeTypeAttribute nam typ crd desc
        