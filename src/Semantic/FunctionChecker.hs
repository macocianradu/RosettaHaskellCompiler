module Semantic.FunctionChecker where

import Model.Function
import Model.Type
import Semantic.ExpressionChecker
import Semantic.TypeChecker
import Data.Either

checkFunction :: ([Type], [Symbol]) ->  Function -> Either [TypeCheckError] Function
checkFunction (definedTypes, definedFunctions) (MakeFunction name desc inp out ex)
    | isRight checkedEx && isRight checkedOut && null (lefts checkedIn) = Right $ MakeFunction name desc (rights checkedIn) (fromRightUnsafe checkedOut) ex
    | otherwise = Left $ lefts [checkedOut] ++ lefts checkedIn ++ lefts [checkedEx]
        where 
            checkedEx = checkExpression definedFunctions ex
            checkedIn = checkAttributes definedTypes inp
            checkedOut = head $ checkAttributes definedTypes [out]