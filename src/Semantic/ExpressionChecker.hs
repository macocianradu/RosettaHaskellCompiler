module Semantic.ExpressionChecker where

import Model.Function
import Data.Either
import Data.Maybe
import Model.Type
import Semantic.TypeChecker
  
data Symbol = Var{
   varName :: String,
   declaredType :: Type
   }
   | Func {
   funcName :: String,
   argsType :: [Type],
   returnType :: Type
   }
  
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
    
checkExpression :: [Symbol] -> Expression -> Either Type TypeCheckError
checkExpression symbolMap (Variable var) = findVarType var symbolMap
checkExpression _ (Int _) = Left $ BasicType "Integer"
checkExpression _ (Real _) = Left $ BasicType "Double"
checkExpression _ (Boolean _) = Left $ BasicType "Boolean"
checkExpression _ Empty = Left $ BasicType "Empty"
checkExpression symbolMap (Parens ex) = checkExpression symbolMap ex
checkExpression symbolMap (List lst) = checkList symbolMap lst 
checkExpression symbolMap (PrefixExp name ex) = checkFunctionCall symbolMap name [checkExpression symbolMap ex]
checkExpression symbolMap (Function name exps) = checkFunctionCall symbolMap name (map (checkExpression symbolMap) exps)
checkExpression symbolMap (PostfixExp name ex) = checkFunctionCall symbolMap name [checkExpression symbolMap ex]
checkExpression symbolMap (InfixExp name ex1 ex2) = checkFunctionCall symbolMap name (checkExpression symbolMap ex1: [checkExpression symbolMap ex2])
checkExpression symbolMap (IfSimple cond ex)
    | isLeft condType && isLeft (typeMatch (fromLeftUnsafe condType) (BasicType "Boolean")) = checkExpression symbolMap ex
    | otherwise = Right IfConditionNotBoolean
    where condType = checkExpression symbolMap cond
checkExpression symbolMap (IfElse cond ex1 ex2)
    | isRight condType || isRight (typeMatch (fromLeftUnsafe condType) (BasicType "Boolean")) = Right IfConditionNotBoolean 
    | isRight ex1Type || isRight ex2Type || isRight (typeMatch (fromLeftUnsafe ex1Type) (fromLeftUnsafe ex2Type)) = Right IfExpressionsDifferentTypes
    | otherwise = ex1Type
    where   condType = checkExpression symbolMap cond
            ex1Type = checkExpression symbolMap ex1
            ex2Type = checkExpression symbolMap ex2

checkList :: [Symbol] -> [Expression] -> Either Type TypeCheckError
checkList symbs exps
    | isLeft typ && fromLeftUnsafe typ == BasicType "Any" = Left $ BasicType "Empty"
    | otherwise = typ
    where typ = checkList1 symbs exps (BasicType "Any") 

checkList1 :: [Symbol] -> [Expression] -> Type -> Either Type TypeCheckError
checkList1 _ [] typ = Left typ
checkList1 symbs (ex : exps) typ 
    | isRight exTyp = exTyp
    | isRight match = match
    | otherwise = checkList1 symbs exps (fromLeftUnsafe match)
    where 
        exTyp = checkExpression symbs ex 
        match = typeMatch typ (fromLeftUnsafe exTyp)

checkFunctionCall :: [Symbol] -> String -> [Either Type TypeCheckError] -> Either Type TypeCheckError
checkFunctionCall [] fun args = Right $ UndefinedFunction $ "Undefined function: \"" ++ fun ++ "\" [" ++ concatMap typeName (lefts args) ++ "]"
checkFunctionCall ((Func n a r):symbolMap) name args
    | length left /= length args = Right ErrorInsideFunction
    | name == n && all isLeft (zipWith typeMatch a left) = Left r
    | otherwise = checkFunctionCall symbolMap name args
    where left = lefts args
checkFunctionCall (_:symbolMap) name args = checkFunctionCall symbolMap name args 

--Try to match 2nd type to first type
typeMatch :: Type -> Type -> Either Type TypeCheckError
typeMatch (BasicType "Any") x = Left x
typeMatch (BasicType "Double") (BasicType "Integer") = Left $ BasicType "Dobule"
typeMatch s (BasicType s2)
    | s == BasicType s2 = Left s
    | otherwise = Right $ TypeMismatch (typeName s) s2
typeMatch s s2
    | s == s2 = Left s
    | isJust $ superType s2 = typeMatch s (fromJust $ superType s2)
    | otherwise = Right $ TypeMismatch (typeName s) (typeName s2)

findVarType :: String -> [Symbol] -> Either Type TypeCheckError
findVarType var [] = Right $ UndefinedVariable var
findVarType x ((Var name typ):symbols) 
    | x == name = Left typ
    | otherwise = findVarType x symbols
findVarType x (_:symbols) = findVarType x symbols

fromRightUnsafe :: Either a b -> b
fromRightUnsafe x = case x of
    Left _ -> error "Value is Left"
    Right b -> b
    
fromLeftUnsafe :: Either a b -> a
fromLeftUnsafe x = case x of
    Left a -> a
    Right _ -> error "Value is Right"