module Semantic.TypeChecker where

import Model.Type
import Model.Function

data Symbol = Var{
  varName :: String,
  declaredType :: String
  }
  | Func {
  funcName :: String,
  argsType :: [String],
  returnType :: String
  }

defaultMap :: [Symbol]
defaultMap = [
  Func "or" ["Boolean", "Boolean"] "Boolean",
  Func "and" ["Boolean", "Boolean"] "Boolean",
  Func "exists" ["Any"] "Boolean",
  Func "is absent" ["Any"] "Boolean",
  Func "single exists" ["Any"] "Boolean",
  Func "multiple exists" ["Any"] "Boolean",
  Func "contains" ["Any", "Any"] "Boolean",
  Func "disjoint" ["Any", "Any"] "Boolean",
  
  Func "=" ["Any", "Any"] "Boolean",
  Func ">=" ["Any", "Any"] "Boolean",
  Func "<=" ["Any", "Any"] "Boolean",
  Func "<>" ["Any", "Any"] "Boolean",
  Func ">" ["Any", "Any"] "Boolean",
  Func "<" ["Any", "Any"] "Boolean",
  Func "all =" ["Any", "Any"] "Boolean",
  Func "all <>" ["Any", "Any"] "Boolean",
  Func "any =" ["Any", "Any"] "Boolean",
  Func "any <>" ["Any", "Any"] "Boolean",
  
  Func "+" ["Integer", "Integer"] "Integer",
  Func "+" ["Double", "Double"] "Double",
  Func "-" ["Integer", "Integer"] "Integer",
  Func "-" ["Double", "Double"] "Double",
  Func "*" ["Integer", "Integer"] "Integer",
  Func "*" ["Double", "Double"] "Double",
  Func "/" ["Integer", "Integer"] "Integer",
  Func "/" ["Double", "Double"] "Double",
  Func "^" ["Integer", "Integer"] "Integer",
  Func "^" ["Double", "Double"] "Double",
  
  Func "count" ["Any"] "Integer"
  ]

checkAttributes :: [String] -> [TypeAttribute] -> [String]
checkAttributes _ [] = []
checkAttributes definedTypes ((MakeTypeAttribute _ name _ _):as) = checkType definedTypes name : checkAttributes definedTypes as 

checkType :: [String] -> String -> String
checkType _ "int" = "Integer"
checkType _ "string" = "String"
checkType _ "boolean" = "Bool"
checkType _ "time" = "Time"
checkType _ "number" = "Double"
checkType definedTypes name
    | name `elem` definedTypes = name
    | otherwise = error "Undefined type: " ++ name
  
addDefinedTypes :: [String] -> [Type] -> [String]
addDefinedTypes l [] = l
addDefinedTypes l ((MakeType name _ _):ts) = name : addDefinedTypes l ts

--Variable String
--    | Int String
--    | Real String
--    | Boolean String
--    | Empty
--    | Parens Expression
--    | List [Expression]
--    | Function String [Expression]
--    | PrefixExp String Expression
--    | PostfixExp String Expression
--    | InfixExp String Expression Expression
--    | IfSimple Expression Expression
--    | IfElse Expression Expression Expression
    
checkExpression :: [Symbol] -> Expression -> String
checkExpression symbolMap (Variable var) = findVar var symbolMap
checkExpression _ (Int _) = "Integer"
checkExpression _ (Real _) = "Double"
checkExpression _ (Boolean _) = "Boolean"
checkExpression _ Empty = "Empty"
checkExpression symbolMap (Parens ex) = checkExpression symbolMap ex
checkExpression _ (List _) = "List" 
checkExpression symbolMap (PrefixExp name ex) = checkFunctionCall symbolMap name [checkExpression symbolMap ex]
checkExpression symbolMap (Function name exps) = checkFunctionCall symbolMap name (map (checkExpression symbolMap) exps)
checkExpression symbolMap (PostfixExp name ex) = checkFunctionCall symbolMap name [checkExpression symbolMap ex]
checkExpression symbolMap (InfixExp name ex1 ex2) = checkFunctionCall symbolMap name (checkExpression symbolMap ex1: [checkExpression symbolMap ex2])
checkExpression symbolMap (IfSimple cond ex)
    | condType == "Boolean" = checkExpression symbolMap ex
    | otherwise = error "Expected boolean condition in if statement"
    where condType = checkExpression symbolMap cond
checkExpression symbolMap (IfElse cond ex1 ex2)
    | condType /= "Boolean" = error "Expected boolean condition in if statement" 
    | not (typeMatch ex1Type ex2Type) = error "Types of then and else branches don't match"
    | otherwise = ex1Type
    where   condType = checkExpression symbolMap cond
            ex1Type = checkExpression symbolMap ex1
            ex2Type = checkExpression symbolMap ex2
    

checkFunctionCall :: [Symbol] -> String -> [String] -> String
checkFunctionCall [] fun args = error "Undefined function: " ++ fun ++ concat args
checkFunctionCall ((Func n a r):symbolMap) name args
    | name == n && and (zipWith typeMatch a args) = r
    | otherwise = checkFunctionCall symbolMap name args
checkFunctionCall (_:symbolMap) name args = checkFunctionCall symbolMap name args 

typeMatch :: String -> String -> Bool
typeMatch "Any" _ = True
typeMatch _ "Any" = True
typeMatch s s2 = s == s2

findVar :: String -> [Symbol] -> String
findVar var [] = error "Undefined variable " ++ var
findVar x ((Var name typ):symbols) 
    | x == name = typ
    | otherwise = findVar x symbols
findVar x (_:symbols) = findVar x symbols