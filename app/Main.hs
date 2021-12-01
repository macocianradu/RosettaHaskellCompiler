module Main where

import Parser.Enum
import Parser.Type
import Parser.Function
import Parser.General
import Model.RosettaObject
import qualified Data.Text as Text
import Text.Megaparsec
import PrettyPrinter.Enum
import PrettyPrinter.Type
import PrettyPrinter.Function
import Semantic.TypeChecker
import Semantic.ExpressionChecker
import Semantic.FunctionChecker
import Model.Type
import System.Environment.Blank (getArgs)
import Model.Enum
import Data.Either

-- :set args resources/testAll.rosetta resources/Generated/testAll.hs
-- :l resources/Generated/testAll.hs
-- |Reads a rosetta string from the first input argument and writes a haskell output to the file given as a second argument
main :: IO ()
main = do
    args <- getArgs
    rosettaString <- readFile $ head args
    -- |Parse the string read from the input file
    case parse rosettaParser "" (Text.pack rosettaString) of
        Left errorBundle -> print (errorBundlePretty errorBundle)
        Right objs -> do
            -- |Write the haskell string into the second argument
            writeFile (args !! 1) (printObjects (definedTypes, definedFunctions) objs) 
            where 
                -- |Adds all the function definitions from the file into the symbol table
                definedFunctions = addNewFunctions (definedTypes, defaultMap) objs
                -- |Adds all the new data types into the symbol table 
                definedTypes = addNewTypes [] objs
   
-- |Reads a rosetta string from the first input argument, parses that string and then writes a haskell output to the file given as a second argument   
printObjects :: ([Type], [Symbol]) -> [RosettaObject] -> String
printObjects (t, s) objs
    | null (lefts finalString) = concat $ rights finalString
    | otherwise = error $ show $ lefts finalString
    where finalString = map (printObject (t, s)) objs  
          
-- |Checks the RosettaObject for type errors and then converts it into a haskell string         
printObject :: ([Type], [Symbol]) -> RosettaObject -> Either [TypeCheckError] String
-- |Checks the type and attributes of a type and then converts it
printObject (definedTypes, _) (TypeObject t)
    | isRight checked = Right $ printType $ fromRightUnsafe checked
    | otherwise = Left $ fromLeftUnsafe checked
    where checked = checkType definedTypes t
-- |Enum is converted directly since no type checks are necessary
printObject _ (EnumObject e) = Right $ printEnum e
-- |Checks the function inputs, output and assignment and converts it
printObject (definedTypes, definedFunctions) (FunctionObject fun)
    | isRight checked = Right $ printFunction $ fromRightUnsafe checked
    | otherwise = Left $ fromLeftUnsafe checked
    where 
        checked = checkFunction (definedTypes, definedFunctions) fun

-- |Adds new defined functions into the symbol table
addNewFunctions :: ([Type], [Symbol]) -> [RosettaObject] -> [Symbol]
addNewFunctions (_, s) [] = s
addNewFunctions (t, s) ((FunctionObject f):os)
    | isRight definedFunctions = fromRightUnsafe definedFunctions
    | otherwise = error $ show $ fromLeftUnsafe definedFunctions
    where definedFunctions = addFunction (t, addNewFunctions (t, s) os) f
addNewFunctions (t, s) (_:os) = addNewFunctions (t, s) os

-- |Adds new defined types into the symbol table
addNewTypes :: [Type] -> [RosettaObject] -> [Type]
addNewTypes l [] = l
addNewTypes defined (TypeObject o: os) = addDefinedTypes (addNewTypes defined os) [o] 
addNewTypes defined (EnumObject (MakeEnum name _ _): os) = addDefinedTypes (addNewTypes defined os) [MakeType name Nothing Nothing []]
addNewTypes defined (_ :os) = addNewTypes defined os

-- |Parses any supported Rosetta types into a list of RosettaObject
rosettaParser :: Parser [RosettaObject]
rosettaParser = many (try parseEnum <|> try parseType <|> try parseFunction) <* eof

-- |Reads an enum into a RosettaObject
parseEnum :: Parser RosettaObject
parseEnum = do
    EnumObject <$> enumParser
    
-- |Parse a type into a RosettaObject
parseType :: Parser RosettaObject
parseType = do
    TypeObject <$> typeParser
    
-- |Parse a function into a RosettaObject
parseFunction :: Parser RosettaObject
parseFunction = do
    FunctionObject <$> functionParser