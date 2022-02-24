module Semantic.FunctionChecker where

import Model.Function
import Model.Type
import Semantic.ExpressionChecker
import Semantic.TypeChecker
import Data.Either
import Data.Char
import Utils.Utils

-- |Checks if all the inputs and the output of a function call have valid types, and then checks that the assign-output expression is valid
checkFunction :: ([Type], [Symbol]) ->  Function -> Either [TypeCheckError] Function
checkFunction (definedTypes, symbols) (MakeFunction name desc inp out ex)
    | isRight checkedEx && isRight checkedOut && null (lefts checkedIn) = 
        case typeIncluded (fromRightUnsafe checkedEx) (attributeType $ fromRightUnsafe checkedOut, Model.Type.cardinality out) of
            Right _ -> Right $ MakeFunction (toLower (head name) : tail name) desc (rights checkedIn) (fromRightUnsafe checkedOut) ex
            Left err -> Left [err] 
    | otherwise = Left $ lefts [checkedOut] ++ lefts checkedIn ++ lefts [checkedEx]
        where 
            checkedIn = checkAttributes definedTypes inp
            localEnv = addVariables symbols inp
            checkedEx = checkExpression localEnv ex
            checkedOut = head $ checkAttributes definedTypes [out]