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
checkFunction (definedTypes, symbols) (MakeFunction (MakeFunctionSignature name desc inp out) alias ex) =
    let checkedIn = checkAttributes definedTypes inp in
    if null $ lefts checkedIn
        then
            let symbolTable = addVariables symbols (rights checkedIn) in
            case addAliases definedTypes symbolTable alias of
                Left err -> Left [err]
                Right checkedAlias -> case head $ checkAttributes definedTypes [out] of
                    Left err -> Left [err]
                    Right checkedOut -> case checkAssignment definedTypes (addVariables (fst checkedAlias) [checkedOut]) ex of
                                Left err -> Left err
                                Right checkedEx -> Right $ MakeExplicitFunction (MakeFunctionSignature (toLower (head name) : tail name) desc (rights checkedIn) checkedOut) (snd checkedAlias) checkedEx
        else
            Left $ lefts checkedIn

checkAssignment :: [Type] -> [Symbol] -> [(Expression, Expression)] -> Either [TypeCheckError] [(ExplicitExpression, ExplicitExpression)]
checkAssignment _ _ [] = Right []
checkAssignment defT symbs ((assign, ex): assigns) =
    -- Here we use only tail symbs, beacuse the head of the symbol table is the out variable, and that can't be used in the expression body
    case checkExpression defT (tail symbs) ex of
    Left err -> Left [err]
    Right checkedExp -> case checkExpression defT symbs assign of
        Left err -> Left [err]
        Right checkedA -> case checkAssignment defT symbs assigns of
                Left err -> Left err
                -- Add a final explicit transformation to match the expected output
                Right checked -> case returnCoercion checkedExp `coercionIncluded` returnCoercion checkedA of
                    Left err -> Left [err]
                    Right c -> Right $ (checkedA, changeCoercion checkedExp c) : checked

addAliases :: [Type] -> [Symbol] -> [(String, Expression)] -> Either TypeCheckError ([Symbol], [(String, ExplicitExpression)])
addAliases definedTypes symbolMap [] = Right (symbolMap, [])
addAliases definedTypes symbolMap (alias : aliases) =
    case checkExpression definedTypes symbolMap (snd alias) of
        Left err -> Left err
        Right ex -> case add of
            Left err -> Left err
            Right added -> Right (fst added, (fst alias, ex) : snd added)
            where add = addAliases definedTypes (addVariables symbolMap [MakeTypeAttribute (fst alias) (typeFromExpression ex) (toCardinality $ cardinalityCoercion $ returnCoercion ex) Nothing]) aliases