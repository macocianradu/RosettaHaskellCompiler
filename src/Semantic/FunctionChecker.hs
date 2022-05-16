module Semantic.FunctionChecker where

import Model.Function
import Model.Type
import Semantic.ExpressionChecker
import Semantic.TypeChecker
import Data.Either
import Data.Char
import Utils.Utils

-- |Checks if all the inputs and the output of a function call have valid types, and then checks that the assign-output expression is valid
checkFunction :: ([Type], [Symbol]) ->  Function -> Either [TypeCheckError] ExplicitFunction
checkFunction (definedTypes, symbols) (MakeFunction (MakeFunctionSignature name desc inp out) ex) =
    let checkedIn = checkAttributes definedTypes inp in
    if null $ lefts checkedIn
        then
            case head $ checkAttributes definedTypes [out] of
                Left err -> Left [err]
                Right checkedOut -> case checkAssignment definedTypes (addVariables symbols (checkedOut : rights checkedIn)) ex of
                    Left err -> Left err
                    Right checkedEx -> Right $ MakeExplicitFunction (MakeFunctionSignature (toLower (head name) : tail name) desc (rights checkedIn) checkedOut) checkedEx
        else
            Left $ lefts checkedIn

checkAssignment :: [Type] -> [Symbol] -> [(Expression, Expression)] -> Either [TypeCheckError] [(ExplicitExpression, ExplicitExpression)]
checkAssignment _ _ [] = Right []
checkAssignment defT symbs ((assign, ex): assigns) = 
    case checkExpression defT (tail symbs) ex of
    Left err -> Left [err]
    -- Here we use only tail symbs, beacuse the head of the symbol table is the out variable, and that can't be used in the expression body
    Right checkedExp -> case checkExpression defT symbs assign of
        Left err -> Left [err]
        Right checkedA -> case checkAssignment defT symbs assigns of
            Left err -> Left err
            Right checked -> Right $ (checkedA, checkedExp) : checked