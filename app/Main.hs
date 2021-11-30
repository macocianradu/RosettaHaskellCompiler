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
import Model.Function
import Model.Type
import System.Environment.Blank (getArgs)
import Model.Enum
import Data.Either

-- :set args resources/testAll.rosetta resources/Generated/testAll.hs
main :: IO ()
main = do
    args <- getArgs
    rosettaString <- readFile $ head args
    case parse rosettaParser "" (Text.pack rosettaString) of
        Left errorBundle -> print (errorBundlePretty errorBundle)
        Right objs -> do
            writeFile (args !! 1) (printObjects (definedTypes, definedFunctions) objs) 
            where 
                definedFunctions = addNewFunctions (definedTypes, defaultMap) objs
                definedTypes = addNewTypes [] objs
      
printObjects :: ([Type], [Symbol]) -> [RosettaObject] -> String
printObjects (t, s) objs
    | null (lefts finalString) = concat $ rights finalString
    | otherwise = error $ show $ lefts finalString
    where finalString = map (printObject (t, s)) objs  
                    
printObject :: ([Type], [Symbol]) -> RosettaObject -> Either [TypeCheckError] String
printObject (definedTypes, _) (TypeObject t)
    | isRight checked = Right $ printType $ fromRightUnsafe checked
    | otherwise = Left $ fromLeftUnsafe checked
    where checked = checkType definedTypes t
printObject _ (EnumObject e) = Right $ printEnum e
printObject (definedTypes, definedFunctions) (FunctionObject fun)
    | isRight checked = Right $ printFunction $ fromRightUnsafe checked
    | otherwise = Left $ fromLeftUnsafe checked
    where 
        checked = checkFunction (definedTypes, definedFunctions) fun

addNewFunctions :: ([Type], [Symbol]) -> [RosettaObject] -> [Symbol]
addNewFunctions (_, s) [] = s
addNewFunctions (t, s) ((FunctionObject f):os)
    | isRight definedFunctions = fromRightUnsafe definedFunctions
    | otherwise = error $ show $ fromLeftUnsafe definedFunctions
    where definedFunctions = addFunction (t, addNewFunctions (t, s) os) f
addNewFunctions (t, s) (_:os) = addNewFunctions (t, s) os

addNewTypes :: [Type] -> [RosettaObject] -> [Type]
addNewTypes l [] = l
addNewTypes defined (TypeObject o: os) = addDefinedTypes (addNewTypes defined os) [o] 
addNewTypes defined (EnumObject (MakeEnum name _ _): os) = addDefinedTypes (addNewTypes defined os) [MakeType name Nothing Nothing []]
addNewTypes defined (_ :os) = addNewTypes defined os

rosettaParser :: Parser [RosettaObject]
rosettaParser = many (try parseEnum <|> try parseType <|> try parseFunction) <* eof

parseEnum :: Parser RosettaObject
parseEnum = do
    EnumObject <$> enumParser
    
parseType :: Parser RosettaObject
parseType = do
    TypeObject <$> typeParser
    
parseFunction :: Parser RosettaObject
parseFunction = do
    FunctionObject <$> functionParser

testEnum :: IO()
testEnum = do
    rosettaString <- readFile "resources/Enums/testEnum1.rosetta"
    case parse enumParser "" (Text.pack rosettaString) of
        Left errorBundle -> print errorBundle
        Right enum -> 
            do
                putStrLn $ printEnum enum
                writeFile "resources/Generated/generatedEnum.hs" (printEnum enum) 
        
testTypeParser :: IO()
testTypeParser = do
    rosettaString <- readFile "resources/Types/testType1.rosetta"
    case parse typeParser "" (Text.pack rosettaString) of
        Left errorBundle -> print (errorBundlePretty errorBundle)
        Right typ -> 
            do
                putStrLn $ printType typ
                print typ
                writeFile "resources/Generated/generatedType.hs" (printType typ) 
              
testTypeChecker :: IO ()
testTypeChecker = do
    rosettaString <- readFile "src/TestFiles/testType.rosetta"
    case parse (many typeParser) "" (Text.pack rosettaString) of
        Left errorBundle -> print (errorBundlePretty errorBundle)
        Right typ -> 
            do
                print $ map (checkAttributes definedTypes . typeAttributes) typ
                where definedTypes = addDefinedTypes [] typ
                
testFunc :: IO()
testFunc = do
    rosettaString <- readFile "resources/testFunction.rosetta"
    case parse functionParser "" (Text.pack rosettaString) of
        Left errorBundle -> print (errorBundlePretty errorBundle)
        Right fun -> 
            do
                print $ printFunctionSignature fun
                print (assignment fun)
                writeFile "resources/Generated/generatedFunction.hs" (show $ printFunctionSignature fun) 