module Main where

import Parser.Enum
import Parser.Type
import Parser.Function
import Data.Text
import Text.Megaparsec
import PrettyPrinter.Enum
import PrettyPrinter.Type
import PrettyPrinter.Function
import Parser.Expression
import Model.Function

main :: IO ()
main = do
    rosettaString <- readFile "app/testFile.rosetta"
    putStrLn "rosetta String: " 
    putStrLn rosettaString
    putStrLn "\nFinal enum: \n"
    case parse enumParser "" (pack rosettaString) of
        Left errorBundle -> print (errorBundlePretty errorBundle)
        Right enum -> putStrLn $ printEnum enum

testEnum :: IO()
testEnum = do
    rosettaString <- readFile "src/TestFiles/testEnum.rosetta"
    case parse enumParser "" (pack rosettaString) of
        Left errorBundle -> print (errorBundlePretty errorBundle)
        Right enum -> 
            do
                putStrLn $ printEnum enum
                writeFile "src/TestFiles/typeEnum.hs" (printEnum enum) 
        
testType :: IO()
testType = do
    rosettaString <- readFile "src/TestFiles/testType.rosetta"
    case parse typeParser "" (pack rosettaString) of
        Left errorBundle -> print (errorBundlePretty errorBundle)
        Right typ -> 
            do
                putStrLn $ printType typ
                print typ
                writeFile "src/TestFiles/typeTest.hs" (printType typ) 
                
testFunc :: IO()
testFunc = do
    rosettaString <- readFile "src/TestFiles/testFunction.rosetta"
    case parse functionParser "" (pack rosettaString) of
        Left errorBundle -> print (errorBundlePretty errorBundle)
        Right fun -> 
            do
                print $ printFunctionSignature fun
                print (assignments fun)
                writeFile "src/TestFiles/functionTest.hs" (show $ printFunctionSignature fun) 