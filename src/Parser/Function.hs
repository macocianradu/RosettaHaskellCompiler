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
        fName <- try pascalNameParser
        _ <- lexeme $ char ':'
        fDescription <- optional descriptionParser
        fInput <- inputAttributesParser
        fOutput <- outputAttributeParser
        MakeFunction fName fDescription fInput fOutput <$> assignmentParser

assignmentParser :: Parser Expression
assignmentParser =
    do
        _ <- lexeme $ string "assign-output"
        _ <- lexeme $ char ':'
        expressionParser

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
        nam <- try camelNameParser 
        typ <- try (pascalNameParser <|> camelNameParser)
        crd <- cardinalityParser
        desc <- optional descriptionParser
        return $ MakeTypeAttribute nam (MakeType typ Nothing Nothing []) crd desc
        