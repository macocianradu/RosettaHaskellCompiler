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
        _ <- lexeme $ string "func"
        fName <- pascalNameParser
        _ <- lexeme $ char ':'
        fDescription <- optional descriptionParser
        fInput <- inputAttributesParser
        fOutput <- outputAttributeParser
        fAssignments <- many assignmentParser
        return (MakeFunction fName fDescription fInput fOutput fAssignments)

assignmentParser :: Parser (Expression, Expression)
assignmentParser =
    do
        _ <- lexeme $ string "assign-output"
        name <- expressionParser
        _ <- lexeme $ char ':'
        expr <- expressionParser
        return (name, expr)

inputAttributesParser :: Parser [TypeAttribute]
inputAttributesParser =
    do
        _ <- lexeme $ string "inputs:"
        many $ try attributeParser

outputAttributeParser :: Parser TypeAttribute
outputAttributeParser =
    do
        _ <- lexeme $ string "output:"
        attributeParser
        
attributeParser :: Parser TypeAttribute
attributeParser =
    do
        nam <- camelNameParser 
        typ <- pascalNameParser <|> camelNameParser
        crd <- cardinalityParser
        desc <- optional descriptionParser
        return $ MakeTypeAttribute nam (MakeType typ Nothing Nothing []) crd desc
        