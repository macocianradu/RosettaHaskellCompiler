module Main where

import Parser.Enum
import Parser.Type
import Parser.Function
import qualified Data.Text as Text
import Text.Megaparsec
import PrettyPrinter.Enum
import PrettyPrinter.Type
import PrettyPrinter.Function
import Semantic.TypeChecker
import Model.Function
import Model.Type

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
    rosettaString <- readFile "resources/Enums/testEnum5.rosetta"
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
                print (assignments fun)
                writeFile "resources/Generated/generatedFunction.hs" (show $ printFunctionSignature fun) 