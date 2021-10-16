module Parser.Function where

import Model.Function
import Model.Type (TypeAttribute)
import Parser.Type (typeAttributeParser)
import Text.Megaparsec.Char
import Text.Megaparsec
import Parser.General
import Data.Text

functionParser :: Parser Function
functionParser =
    do
        _ <- string $ pack "func"
        _ <- spaceConsumer
        fName <- pascalNameParser
        _ <- char ':'
        _ <- spaceConsumer
        fDescription <- descriptionParser
        fInput <- inputAttributesParser
        fOutput <- outputAttributeParser
        fConditions <- many (try postConditionParser <|> try conditionParser)
        _ <- spaceConsumer
        return (MakeFunction fName (Just fDescription) fInput fOutput fConditions)

inputAttributesParser :: Parser [TypeAttribute]
inputAttributesParser =
    do
        _ <- string $ pack "inputs:"
        _ <- spaceConsumer
        inputs <- many $ try typeAttributeParser
        _ <- spaceConsumer
        return inputs

outputAttributeParser :: Parser TypeAttribute
outputAttributeParser =
    do
        _ <- string $ pack "output:"
        _ <- spaceConsumer
        outputs <- typeAttributeParser
        _ <- spaceConsumer
        return outputs

--parseTest conditionParser pack "condition: <\"Optional choice between directly passing a time or a timeType, which has to be resolved into a time based on the determination method.\"> if valuationTime exists then timeType is absent else if timeType exists then valuationTime is absent else False"

conditionParser :: Parser Condition
conditionParser =
    do
        _ <- string $ pack "condition:"
        _ <- spaceConsumer
        description <- descriptionParser
        (Expression statementString) <- lookAhead expressionParser
        _ <- string $ pack statementString
        rest <- getInput
        _ <- setInput $ pack statementString
        statement <- statementParser
        _ <- setInput rest
        _ <- spaceConsumer
        return $ MakeCondition (Just description) statement
--parseTest postConditionParser (pack "post-condition: <\"The date and time must be properly resolved as attributes on the output.\"> observation -> date = ResolveAdjustableDate(valuationDate) and if valuationTime exists then observation -> time = TimeZoneFromBusinessCenterTime(valuationTime) else observation -> time = ResolveTimeZoneFromTimeType(timeType, determinationMethod)")

postConditionParser :: Parser Condition
postConditionParser =
    do
        _ <- string $ pack "post-condition:"
        _ <- spaceConsumer
        description <- descriptionParser
        (Expression statementString) <- lookAhead expressionParser
        _ <- string $ pack statementString
        rest <- getInput
        _ <- setInput $ pack statementString
        statement <- statementParser
        _ <- setInput rest
        _ <- spaceConsumer
        return $ MakePostCondition (Just description) statement

statementParser :: Parser Expression
statementParser =
    do
        statement <-
            try ifElseParser <|>
            try ifParser <|>

            try (binaryOpParser " and ") <|>
            try (binaryOpParser " contains ") <|>
            try (binaryOpParser " or ") <|>
            try (binaryOpParser " = ") <|>
            try (binaryOpParser " <> ") <|>
            try (binaryOpParser " < ") <|>
            try (binaryOpParser " <= ") <|>
            try (binaryOpParser " >") <|>
            try (binaryOpParser " >= ") <|>

            try (unaryOpParser " count") <|>
            try (unaryOpParser " exists") <|>
            try (unaryOpParser " is absent") <|>
            
            expressionParser
        _ <- spaceConsumer
        return statement

--binaryOpParser :: String -> Parser Expression
--binaryOpParser op =
--    do
--        argument1String <- untilParser op
--        rest <- getInput
--        _ <- setInput $ pack argument1String
--        argument1 <- statementParser
--        _ <- setInput rest
--        _ <- spaceConsumer
--        argument2 <- statementParser
--        _ <- spaceConsumer
--        return $ BinaryOp (unpack $ strip $ pack op) argument1 argument2
--
--unaryOpParser :: String -> Parser Expression
--unaryOpParser op =
--    do
--        statementString <- untilParser op
--        rest <- getInput
--        _ <- setInput $ pack statementString
--        statement <- statementParser
--        _ <- setInput rest
--        _ <- spaceConsumer
--        return $ UnaryOp (unpack $ strip $ pack op) statement 
--
--ifParser :: Parser Expression
--ifParser =
--    do
--        _ <- string $ pack "if"
--        _ <- spaceConsumer
--        cond <- statementParser
--        _ <- spaceConsumer
--        _ <- string $ pack "then"
--        expr <- statementParser
--        _ <- spaceConsumer
--        return $ IfSimple cond expr

--ifParser :: Parser Expression
--ifParser =
--    do
--        _ <- string $ pack "if"
--        _ <- spaceConsumer
--        conditionString <- untilParser "then"
--        rest <- getInput
--        _ <- setInput $ pack conditionString
--        condition <- statementParser
--        _ <- setInput rest
--        _ <- spaceConsumer
--        stmt <- statementParser
--        return $ If condition stmt

--ifElseParser :: Parser Expression
--ifElseParser =
--    do
--        _ <- string $ pack "if"
--        _ <- spaceConsumer
--        conditionString <- untilParser "then"
--        rest <- getInput
--        _ <- setInput $ pack conditionString
--        condition <- statementParser
--        _ <- setInput rest
--        _ <- spaceConsumer
--        thenString <- untilParser "else"
--        rest1 <- getInput
--        _ <- setInput $ pack thenString
--        thenExpression <- statementParser
--        _ <- setInput rest1
--        _ <- spaceConsumer
--        elseExpression <- statementParser
--        return $ IfElse condition thenExpression elseExpression

--parseTest expressionParser (pack "alalala condition:")

expressionParser :: Parser Expression
expressionParser =
    do
        statement <-
            untilParser "post-condition:" <|>
            untilParser "condition:" <|>
            untilParser "func" <|>
            untilParser "enum" <|>
            untilParser "type" <|>
            try (anySingle `manyTill` eof)
        _ <- spaceConsumer
        return $ Expression $ unpack (strip $ pack statement)