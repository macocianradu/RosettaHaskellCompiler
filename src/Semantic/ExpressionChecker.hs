module Semantic.ExpressionChecker where

import Model.Function
import Data.Either
import Data.Maybe
import Model.Type
import Semantic.TypeChecker
  
-- |A declared variable or function
data Symbol = Var{
   varName :: String,
   declaredType :: Type
   }
   | Func {
   funcName :: String,
   argsType :: [Type],
   returnType :: Type
   }
  
-- |A map of the predefined functions, their arguments and their return type
defaultMap :: [Symbol]
defaultMap = [
  Func "or" [BasicType "Boolean", BasicType "Boolean"] (BasicType "Boolean"),
  Func "and" [BasicType "Boolean", BasicType "Boolean"] (BasicType "Boolean"),
  Func "exists" [BasicType "Any"] (BasicType "Boolean"),
  Func "is absent" [BasicType "Any"] (BasicType "Boolean"),
  Func "single exists" [BasicType "Any"] (BasicType "Boolean"),
  Func "multiple exists" [BasicType "Any"] (BasicType "Boolean"),
  Func "contains" [BasicType "Any", BasicType "Any"] (BasicType "Boolean"),
  Func "disjoint" [BasicType "Any", BasicType "Any"] (BasicType "Boolean"),
  
  Func "=" [BasicType "Any", BasicType "Any"] (BasicType "Boolean"),
  Func ">=" [BasicType "Any", BasicType "Any"] (BasicType "Boolean"),
  Func "<=" [BasicType "Any", BasicType "Any"] (BasicType "Boolean"),
  Func "<>" [BasicType "Any", BasicType "Any"] (BasicType "Boolean"),
  Func ">" [BasicType "Any", BasicType "Any"] (BasicType "Boolean"),
  Func "<" [BasicType "Any", BasicType "Any"] (BasicType "Boolean"),
  Func "all =" [BasicType "Any", BasicType "Any"] (BasicType "Boolean"),
  Func "all <>" [BasicType "Any", BasicType "Any"] (BasicType "Boolean"),
  Func "any =" [BasicType "Any", BasicType "Any"] (BasicType "Boolean"),
  Func "any <>" [BasicType "Any", BasicType "Any"] (BasicType "Boolean"),
  
  Func "+" [BasicType "Integer", BasicType "Integer"] (BasicType "Integer"),
  Func "+" [BasicType "Double", BasicType "Double"] (BasicType "Double"),
  Func "-" [BasicType "Integer", BasicType "Integer"] (BasicType "Integer"),
  Func "-" [BasicType "Double", BasicType "Double"] (BasicType "Double"),
  Func "*" [BasicType "Integer", BasicType "Integer"] (BasicType "Integer"),
  Func "*" [BasicType "Double", BasicType "Double"] (BasicType "Double"),
  Func "/" [BasicType "Integer", BasicType "Integer"] (BasicType "Integer"),
  Func "/" [BasicType "Double", BasicType "Double"] (BasicType "Double"),
  Func "^" [BasicType "Integer", BasicType "Integer"] (BasicType "Integer"),
  Func "^" [BasicType "Double", BasicType "Double"] (BasicType "Double"),
  
  Func "count" [BasicType "Any"] (BasicType "Integer")
  ]

-- |Checks whether a function is valid (inputs, outputs are of valid type and all variables are defined) and adds it to the symbol table
addFunction :: ([Type], [Symbol]) -> Function -> Either [TypeCheckError] [Symbol]
addFunction (definedTypes, definedSymbols) (MakeFunction name _ inps out _)
    | null (lefts checkedInputs) && isRight checkedOutput = Right $ Func name (map attributeType (rights checkedInputs)) (attributeType $ fromRightUnsafe checkedOutput) : allSymbols
    | isLeft checkedOutput = Left [fromLeftUnsafe checkedOutput]
    | otherwise = Left $  lefts checkedInputs 
    where 
        checkedInputs = checkAttributes definedTypes inps
        checkedOutput = head $ checkAttributes definedTypes [out]
        allSymbols = addVariables definedSymbols inps
        
-- |Adds a newly defined variable to the symbol table
addVariables :: [Symbol] -> [TypeAttribute] -> [Symbol]
addVariables s [] = s
addVariables s ((MakeTypeAttribute name typ _ _)  : vars) = Var name typ : addVariables s vars
        
-- |Checks the type of a given expression
checkExpression :: [Symbol] -> Expression -> Either TypeCheckError Type
checkExpression symbolMap (Variable var) = findVarType var symbolMap
checkExpression _ (Int _) = Right $ BasicType "Integer"
checkExpression _ (Real _) = Right $ BasicType "Double"
checkExpression _ (Boolean _) = Right $ BasicType "Boolean"
checkExpression _ Empty = Right $ BasicType "Empty"
checkExpression symbolMap (Parens ex) = checkExpression symbolMap ex
checkExpression symbolMap (List lst) = checkList symbolMap lst 
checkExpression symbolMap (PrefixExp name ex) = checkFunctionCall symbolMap name [checkExpression symbolMap ex]
checkExpression symbolMap (Function name exps) = checkFunctionCall symbolMap name (map (checkExpression symbolMap) exps)
checkExpression symbolMap (PostfixExp name ex) = checkFunctionCall symbolMap name [checkExpression symbolMap ex]
checkExpression symbolMap (InfixExp name ex1 ex2) = checkFunctionCall symbolMap name (checkExpression symbolMap ex1: [checkExpression symbolMap ex2])
-- |Checks if the condition of an if expression is of type boolean, and then checks the expression of the then statement
checkExpression symbolMap (IfSimple cond ex)
    | isRight condType && isRight (typeMatch (fromRightUnsafe condType) (BasicType "Boolean")) = checkExpression symbolMap ex
    | otherwise = Left IfConditionNotBoolean
    where condType = checkExpression symbolMap cond
-- |Checks if the condition of the if statement is of type boolean, and then checks that both the then and else statements have the same type
checkExpression symbolMap (IfElse cond ex1 ex2)
    | isRight condType || isRight (typeMatch (fromRightUnsafe condType) (BasicType "Boolean")) = Left IfConditionNotBoolean 
    | isRight ex1Type || isRight ex2Type || isRight (typeMatch (fromRightUnsafe ex1Type) (fromRightUnsafe ex2Type)) = Left IfExpressionsDifferentTypes
    | otherwise = ex1Type
    where   condType = checkExpression symbolMap cond
            ex1Type = checkExpression symbolMap ex1
            ex2Type = checkExpression symbolMap ex2

-- |Checks that all the expressions in a list have compatible types
checkList :: [Symbol] -> [Expression] -> Either TypeCheckError Type
checkList symbs exps
    | isRight typ && fromRightUnsafe typ == BasicType "Any" = Right $ BasicType "Empty"
    | otherwise = typ
    where typ = checkList1 symbs exps (BasicType "Any") 

-- |Auxiliary function for the check list function
checkList1 :: [Symbol] -> [Expression] -> Type -> Either TypeCheckError Type
checkList1 _ [] typ = Right typ
checkList1 symbs (ex : exps) typ 
    | isRight exTyp = exTyp
    | isRight match = match
    | otherwise = checkList1 symbs exps (fromRightUnsafe match)
    where 
        exTyp = checkExpression symbs ex 
        match = typeMatch typ (fromRightUnsafe exTyp)

-- |Checks whether the function that is called is already defined with the same argument types
checkFunctionCall :: [Symbol] -> String -> [Either TypeCheckError Type] -> Either TypeCheckError Type
checkFunctionCall [] fun args = Left $ UndefinedFunction $ "Undefined function: \"" ++ fun ++ "\" [" ++ concatMap typeName (rights args) ++ "]"
checkFunctionCall ((Func n a r):symbolMap) name args
    | length right /= length args = Left $ ErrorInsideFunction (name ++ ": " ++ show args)
    | name == n && all isRight (zipWith typeMatch a right) = Right r
    | otherwise = checkFunctionCall symbolMap name args
    where right = rights args
checkFunctionCall (_:symbolMap) name args = checkFunctionCall symbolMap name args 

--Try to match 2nd type to first type
-- |Checks whether two types are compatible
typeMatch :: Type -> Type -> Either TypeCheckError Type
typeMatch (BasicType "Any") x = Right x
typeMatch (BasicType "Double") (BasicType "Integer") = Right $ BasicType "Dobule"
typeMatch s (BasicType s2)
    | s == BasicType s2 = Right s
    | otherwise = Left $ TypeMismatch (typeName s) s2
typeMatch s s2
    | s == s2 = Right s
    | isJust $ superType s2 = typeMatch s (fromJust $ superType s2)
    | otherwise = Left $ TypeMismatch (typeName s) (typeName s2)

-- |Looks in the symbol map for the type of a variable
findVarType :: String -> [Symbol] -> Either TypeCheckError Type
findVarType var [] = Left $ UndefinedVariable var
findVarType x ((Var name typ):symbols) 
    | x == name = Right typ
    | otherwise = findVarType x symbols
findVarType x (_:symbols) = findVarType x symbols