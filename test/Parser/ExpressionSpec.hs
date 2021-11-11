{-# LANGUAGE OverloadedStrings #-}

module Parser.ExpressionSpec where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Model.Function
import Parser.Expression
  
spec :: Spec
spec = do
    describe "Testing expression parsing" $ do
        it "[Test 1]" $ do
            parse expressionParser "" "1 + (2 - 3)" `shouldParse` head exps
        it "[Test 2]" $ do
            parse expressionParser "" "a * b - c * d - e * f = g * h - i * j - k * l" `shouldParse` (exps !! 1) 
        it "[Test 3]" $ do
            parse expressionParser "" "a + b - c * d ^ e" `shouldParse` (exps !! 2) 
        it "[Test 4]" $ do
            parse expressionParser "" "1 - 2 - 3 - 4 - 5 - 6" `shouldParse` (exps !! 3) 
        it "[Test 5]" $ do
            parse expressionParser "" "[1, 2, 3]" `shouldParse` (exps !! 4) 
        it "[Test 6]" $ do
            parse expressionParser "" "[1, 2 + 3, e]" `shouldParse` (exps !! 5) 
        it "[Test 7]" $ do
            parse expressionParser "" "Function()" `shouldParse` (exps !! 6) 
        it "[Test 8]" $ do
            parse expressionParser "" "Function(e)" `shouldParse` (exps !! 7) 
        it "[Test 9]" $ do
            parse expressionParser "" "Function(3, 3+2,e)" `shouldParse` (exps !! 8) 
        it "[Test 10]" $ do
            parse expressionParser "" "if (Function(2 + 3, e)) then a + b - c * d ^ e -> x else not a exists" `shouldParse` (exps !! 9) 
        it "[Test 11]" $ do
            parse expressionParser "" "if [1, Function(3)] then 1 - 2 - 3 * a -> b ^ c" `shouldParse` (exps !! 10) 
        it "[Test 12]" $ do
            parse expressionParser "" "a or b" `shouldParse` (exps !! 11) 
        it "[Test 13]" $ do
            parse expressionParser "" `shouldFailOn` "if[1,as]thenxandoelseaora"

exps :: [Expression]
exps = [
    InfixExp "+" (Int "1") (Parens (InfixExp "-" (Int "2") (Int "3"))),
    InfixExp "=" 
        (InfixExp "-" 
            (InfixExp "-" 
                (InfixExp "*" (Variable "a") (Variable "b"))
                (InfixExp "*" (Variable "c") (Variable "d")))
            (InfixExp "*" (Variable "e") (Variable "f")))
        (InfixExp "-" 
            (InfixExp "-" 
                (InfixExp "*" (Variable "g") (Variable "h"))
                (InfixExp "*" (Variable "i") (Variable "j")))
            (InfixExp "*" (Variable "k") (Variable "l"))),
    InfixExp "-" (InfixExp "+" (Variable "a") (Variable "b")) (InfixExp "*" (Variable "c") (InfixExp "^" (Variable "d") (Variable "e"))),
    InfixExp "-" (InfixExp "-" (InfixExp "-" (InfixExp "-" (InfixExp "-" (Int "1") (Int "2")) (Int "3")) (Int "4")) (Int "5")) (Int "6"),
    List [Int "1", Int "2", Int "3"],
    List [Int "1", InfixExp "+" (Int "2") (Int "3"), Variable "e"],
    Function "Function" [],
    Function "Function" [Variable "e"],
    Function "Function" [Int "3", InfixExp "+" (Int "3") (Int "2"), Variable "e"],
    IfElse (Function "Function" [InfixExp "+" (Int "2") (Int "3"), Variable "e"]) 
         (InfixExp "-" (InfixExp "+" (Variable "a") (Variable "b")) (InfixExp "*" (Variable "c") (InfixExp "^" (Variable "d") (Variable "e->x"))))
         (PrefixExp "not" (PostfixExp "exists" (Variable "a"))),
    IfSimple (List [Int "1", Function "Function" [Int "3"]]) (InfixExp "-" (InfixExp "-" (Int "1") (Int "2")) (InfixExp "*" (Int "3") (InfixExp "^" (Variable "a->b") (Variable "c")))),
    InfixExp "or" (Variable "a") (Variable "b")
  ]
                
--mapExpsToTypes :: [String] -> [(String, String)]
--mapExpsToTypes [] = []
--mapExpsToTypes (expr: exps) = do
--    case parse expressionParser "" (Text.pack expr) of
--        Left errorBundle -> error (errorBundlePretty errorBundle)
--        Right ex -> (show ex, show $ checkExpression defaultMap ex) :mapExpsToTypes exps
--        
--printOnOneLine :: [(String, String)] -> String
--printOnOneLine [] = ""
--printOnOneLine ((ex, typ): exps) = "(" ++ ex ++ "," ++ typ ++ ")\n" ++ printOnOneLine exps 
--
--           
--expressions :: [String]
--expressions = [
--  --Or Good
--  "True or False",
--  --Or Bad
--  "1 or False",
--  --And Good
--  "False and False",
--  --And Bad
--  "1 and 2",
--  --Exists Good
--  "a exists",
--  --Plus Good
--  "1.2 + 2.3",
--  "1 + 2.3",
--  "2.3 + 1",
--  "1 + 2",
--  --Plus Bad
--  "True + 2",
--  --If Good
--  "if True then 2 else 3",
--  --If Bad Cond
--  "if 2 then True else False",
--  --If Bad exps
--  "if True then 2 else False",
--  "if True or False then 24 + 15 else 55 + 98 + 35 + 34",
--  --List Good
--  "[2, 3]",
--  --List Bad
--  "[22, False, 5]"
--  ]