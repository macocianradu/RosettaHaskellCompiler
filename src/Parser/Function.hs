{-# LANGUAGE OverloadedStrings #-}

module Parser.Function where

import Parser.Expression
import Parser.Type
import Model.Function
import Model.Type
import Text.Megaparsec
import Text.Megaparsec.Char
import Parser.General
  
-- |Parses a function statement in Rosetta into a Function
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

-- |Parses the output assignment statement from a function in Rosetta into an Expression
assignmentParser :: Parser Expression
assignmentParser =
    do
        _ <- lexeme $ string "assign-output"
        _ <- lexeme $ char ':'
        expressionParser

-- |Parses the input attributes from a function statement in Rosetta into a list of TypeAttributes
inputAttributesParser :: Parser [TypeAttribute]
inputAttributesParser =
    do
        _ <- lexeme $ string "inputs:"
        many $ try attributeParser

-- |Parses the output attribute of a function statement in Rosetta into a TypeAttribute
outputAttributeParser :: Parser TypeAttribute
outputAttributeParser =
    do
        _ <- lexeme $ string "output:"
        attributeParser
        
-- |Auxiliary function that parses an attribute into a TypeAttribute
attributeParser :: Parser TypeAttribute
attributeParser =
    do
        nam <- try camelNameParser 
        typ <- try (pascalNameParser <|> camelNameParser)
        crd <- cardinalityParser
        desc <- optional descriptionParser
        return $ MakeTypeAttribute nam (MakeType typ Nothing Nothing []) crd desc
        