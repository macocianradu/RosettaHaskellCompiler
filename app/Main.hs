module Main where

import Parser.Enum
import Parser.Type
import Parser.Function
import qualified Data.Text as Text
import Text.Megaparsec
import PrettyPrinter.Enum
import PrettyPrinter.Type
import PrettyPrinter.Function
import Parser.Expression
import Semantic.TypeChecker
import Model.Function
import Model.Type (typeAttributes)

main :: IO ()
main = do
    rosettaString <- readFile "app/testFile.rosetta"
    putStrLn "rosetta String: " 
    putStrLn rosettaString
    putStrLn "\nFinal enum: \n"
    case parse enumParser "" (Text.pack rosettaString) of
        Left errorBundle -> print (errorBundlePretty errorBundle)
        Right enum -> putStrLn $ printEnum enum

testEnum :: IO()
testEnum = do
    rosettaString <- readFile "src/TestFiles/testEnum.rosetta"
    case parse enumParser "" (Text.pack rosettaString) of
        Left errorBundle -> print (errorBundlePretty errorBundle)
        Right enum -> 
            do
                putStrLn $ printEnum enum
                writeFile "src/TestFiles/typeEnum.hs" (printEnum enum) 
        
testTypeParser :: IO()
testTypeParser = do
    rosettaString <- readFile "src/TestFiles/testType.rosetta"
    case parse typeParser "" (Text.pack rosettaString) of
        Left errorBundle -> print (errorBundlePretty errorBundle)
        Right typ -> 
            do
                putStrLn $ printType typ
                print typ
                writeFile "src/TestFiles/typeTest.hs" (printType typ) 
              
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
    rosettaString <- readFile "src/TestFiles/testFunction.rosetta"
    case parse functionParser "" (Text.pack rosettaString) of
        Left errorBundle -> print (errorBundlePretty errorBundle)
        Right fun -> 
            do
                print $ printFunctionSignature fun
                print (assignments fun)
                writeFile "src/TestFiles/functionTest.hs" (show $ printFunctionSignature fun) 
           
testExpTypeChecker :: IO ()
testExpTypeChecker = print $ mapExpsToTypes expressions
                
mapExpsToTypes :: [String] -> [(String, String)]
mapExpsToTypes [] = []
mapExpsToTypes (expr: exps) = do
    case parse expressionParser "" (Text.pack expr) of
        Left errorBundle -> error (errorBundlePretty errorBundle)
        Right ex -> (show ex, checkExpression defaultMap ex) :mapExpsToTypes exps
            
expressions :: [String]
expressions = [
  --Or Good
  "True or False",
--  --Or Bad
--  "1 or False",
  --And Good
  "False and False",
--  --And Bad
--  "1 and 2",
  --Exists Good
  "a exists",
  --Plus Good
  "1.2 + 2.3",
--  --Plus Bad
--  "True + 2",
  --If Good
  "if True then 2 else 3",
--  --If Bad Cond
--  "if 2 then True else False",
  --If Bad exps
--  "if True then 2 else False"
    "if True or False then 24 + 15 else 55 + 98 + 35 + 34"
  ]