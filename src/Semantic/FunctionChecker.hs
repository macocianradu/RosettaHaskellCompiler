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
    if null $ lefts checkedIn
        then
            case head $ checkAttributes definedTypes [out] of
                Left err -> Left [err]
                Right checkedOut -> case checkExpression (addVariables symbols inp) ex of
                    Left err -> Left [err]
                    Right checkedEx -> case returnCoercion checkedEx `coercionIncluded` createCoercion (attributeType checkedOut, Model.Type.cardinality out) of
                        Left err -> Left [err]
                        Right retCoercion -> Right $ MakeExplicitFunction (MakeFunctionSignature (toLower (head name) : tail name) desc (rights checkedIn) checkedOut) checkedEx
                        --Right _ -> error $ show (returnCoercion checkedEx) ++ " // " ++ show (createCoercion (attributeType checkedOut, Model.Type.cardinality out))
        else
            Left $ lefts checkedIn
    where checkedIn = checkAttributes definedTypes inp