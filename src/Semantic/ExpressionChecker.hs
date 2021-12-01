module Semantic.ExpressionChecker where

import Model.Function
import Data.Either
import Data.Maybe
import Model.Type
import Semantic.TypeChecker
  
-- |A declared variable or function
data Symbol = Var{
   varName :: String,
   declaredType :: Type,
   cardinality :: Cardinality
   }
   | Func {
   funcName :: String,
   argsType :: [(Type, Cardinality)],
   returnType :: (Type, Cardinality)
   } deriving (Show)
  
  
-- |A map of the predefined functions, their arguments and their return type
defaultMap :: [Symbol]
defaultMap = [
  Func "or" [(BasicType "Boolean", Bounds (1, 1)), (BasicType "Boolean", Bounds (1, 1))] (BasicType "Boolean", Bounds (1, 1)),
  Func "and" [(BasicType "Boolean", Bounds (1, 1)), (BasicType "Boolean", Bounds (1, 1))] (BasicType "Boolean", Bounds (1, 1)),
  Func "exists" [(BasicType "Any", Bounds (0, 1))] (BasicType "Boolean", Bounds(1, 1)),
  Func "is absent" [(BasicType "Any", Bounds (0, 1))] (BasicType "Boolean", Bounds (1, 1)),
  Func "single exists" [(BasicType "Any", NoBounds)] (BasicType "Boolean", Bounds (1, 1)),
  Func "multiple exists" [(BasicType "Any", NoBounds)] (BasicType "Boolean", Bounds (1, 1)),
  Func "contains" [(BasicType "Any", NoBounds), (BasicType "Any", NoBounds)] (BasicType "Boolean", Bounds (1, 1)),
  Func "disjoint" [(BasicType "Any", NoBounds), (BasicType "Any", NoBounds)] (BasicType "Boolean", Bounds (1, 1)),
  
  Func "=" [(BasicType "Any", NoBounds), (BasicType "Any", NoBounds)] (BasicType "Boolean", Bounds(1, 1)),
  Func ">=" [(BasicType "Any", NoBounds), (BasicType "Any", NoBounds)] (BasicType "Boolean", Bounds(1, 1)),
  Func "<=" [(BasicType "Any", NoBounds), (BasicType "Any", NoBounds)] (BasicType "Boolean", Bounds(1, 1)),
  Func "<>" [(BasicType "Any", NoBounds), (BasicType "Any", NoBounds)] (BasicType "Boolean", Bounds(1, 1)),
  Func ">" [(BasicType "Any", NoBounds), (BasicType "Any", NoBounds)] (BasicType "Boolean", Bounds(1, 1)),
  Func "<" [(BasicType "Any", NoBounds), (BasicType "Any", NoBounds)] (BasicType "Boolean", Bounds(1, 1)),
  Func "all =" [(BasicType "Any", NoBounds), (BasicType "Any", Bounds(1, 1))] (BasicType "Boolean", Bounds(1, 1)),
  Func "all <>" [(BasicType "Any", NoBounds), (BasicType "Any", Bounds(1, 1))] (BasicType "Boolean", Bounds(1, 1)),
  Func "any =" [(BasicType "Any", NoBounds), (BasicType "Any", Bounds(1, 1))] (BasicType "Boolean", Bounds(1, 1)),
  Func "any <>" [(BasicType "Any", NoBounds), (BasicType "Any", Bounds(1, 1))] (BasicType "Boolean", Bounds(1, 1)),
  
  Func "+" [(BasicType "Integer", Bounds (1, 1)), (BasicType "Integer", Bounds (1, 1))] (BasicType "Integer", Bounds (1, 1)),
  Func "+" [(BasicType "Double", Bounds (1, 1)), (BasicType "Double", Bounds (1, 1))] (BasicType "Double", Bounds (1, 1)),
  Func "-" [(BasicType "Integer", Bounds (1, 1)), (BasicType "Integer", Bounds (1, 1))] (BasicType "Integer", Bounds (1, 1)),
  Func "-" [(BasicType "Double", Bounds (1, 1)), (BasicType "Double", Bounds (1, 1))] (BasicType "Double", Bounds (1, 1)),
  Func "*" [(BasicType "Integer", Bounds (1, 1)), (BasicType "Integer", Bounds (1, 1))] (BasicType "Integer", Bounds (1, 1)),
  Func "*" [(BasicType "Double", Bounds (1, 1)), (BasicType "Double", Bounds (1, 1))] (BasicType "Double", Bounds (1, 1)),
  Func "/" [(BasicType "Integer", Bounds (1, 1)), (BasicType "Integer", Bounds (1, 1))] (BasicType "Integer", Bounds (1, 1)),
  Func "/" [(BasicType "Double", Bounds (1, 1)), (BasicType "Double", Bounds (1, 1))] (BasicType "Double", Bounds (1, 1)),
  Func "^" [(BasicType "Integer", Bounds (1, 1)), (BasicType "Integer", Bounds (1, 1))] (BasicType "Integer", Bounds (1, 1)),
  Func "^" [(BasicType "Double", Bounds (1, 1)), (BasicType "Double", Bounds (1, 1))] (BasicType "Double", Bounds (1, 1)),
  
  Func "count" [(BasicType "Any", NoBounds)] (BasicType "Integer", Bounds (1, 1))
  ]

-- |Checks whether a function is valid (inputs, outputs are of valid type and all variables are defined) and adds it to the symbol table
addFunction :: ([Type], [Symbol]) -> Function -> Either [TypeCheckError] [Symbol]
addFunction (definedTypes, definedSymbols) (MakeFunction name _ inps out _)
    | null (lefts checkedInputs) && isRight checkedOutput = Right $ Func name (map typeAndCardinality (rights checkedInputs)) (attributeType $ fromRightUnsafe checkedOutput, Model.Type.cardinality out) : allSymbols
    | isLeft checkedOutput = Left [fromLeftUnsafe checkedOutput]
    | otherwise = Left $  lefts checkedInputs 
    where 
        checkedInputs = checkAttributes definedTypes inps
        checkedOutput = head $ checkAttributes definedTypes [out]
        allSymbols = addVariables definedSymbols inps
        
-- |Adds a newly defined variable to the symbol table
addVariables :: [Symbol] -> [TypeAttribute] -> [Symbol]
addVariables s [] = s
addVariables s ((MakeTypeAttribute name typ crd _)  : vars) = Var name typ crd : addVariables s vars
        
-- |Checks the type of a given expression
checkExpression :: [Symbol] -> Expression -> Either TypeCheckError (Type, Cardinality)
checkExpression symbolMap (Variable var) = findVarType var symbolMap
checkExpression _ (Int _) = Right (BasicType "Integer", Bounds (1, 1))
checkExpression _ (Real _) = Right (BasicType "Double", Bounds (1, 1))
checkExpression _ (Boolean _) = Right (BasicType "Boolean", Bounds (1, 1))
checkExpression _ Empty = Right (BasicType "Empty", Bounds (0, 0))
checkExpression symbolMap (Parens ex) = checkExpression symbolMap ex
checkExpression symbolMap (List lst) = checkList symbolMap lst 
checkExpression symbolMap (PrefixExp name ex) = checkFunctionCall symbolMap name [checkExpression symbolMap ex]
checkExpression symbolMap (Function name exps) = checkFunctionCall symbolMap name (map (checkExpression symbolMap) exps)
checkExpression symbolMap (PostfixExp name ex) = checkFunctionCall symbolMap name [checkExpression symbolMap ex]
checkExpression symbolMap (InfixExp name ex1 ex2) = checkFunctionCall symbolMap name (checkExpression symbolMap ex1: [checkExpression symbolMap ex2])
-- |Checks if the condition of an if expression is of type boolean, and then checks the expression of the then statement
checkExpression symbolMap (IfSimple cond ex)
    | isRight condType && isRight (typeMatch (fromRightUnsafe condType) (BasicType "Boolean", Bounds (1, 1))) = 
        case checkedExp of
            -- |The if without else statement always has a cardinality lower bound of 0
            Right (typ, Bounds(_, x)) -> Right (typ, Bounds(0, x))
            -- |The unbounded or semi-bounded cardinalities already have 0 included
            Right x -> Right x
            Left err -> Left err
    | otherwise = Left $ IfConditionNotBoolean $ show condType
    where 
        condType = checkExpression symbolMap cond
        checkedExp = checkExpression symbolMap ex
-- |Checks if the condition of the if statement is of type boolean, and then checks that both the then and else statements have the same type
checkExpression symbolMap (IfElse cond ex1 ex2)
    | isRight condType || isRight (typeMatch (fromRightUnsafe condType) (BasicType "Boolean", Bounds (1, 1))) = Left $ IfConditionNotBoolean $ show cond
    | isRight ex1Type || isRight ex2Type || isRight (typeMatch (fromRightUnsafe ex1Type) (fromRightUnsafe ex2Type)) = Left $ IfExpressionsDifferentTypes (show ex1) (show ex2)
    | otherwise = ex1Type
    where   condType = checkExpression symbolMap cond
            ex1Type = checkExpression symbolMap ex1
            ex2Type = checkExpression symbolMap ex2

-- |Checks that all the expressions in a list have compatible types
checkList :: [Symbol] -> [Expression] -> Either TypeCheckError (Type, Cardinality)
checkList symbs exps
    | isRight typ && fromRightUnsafe typ == (BasicType "Any", NoBounds) = Right (BasicType "Empty", Bounds (0, 0))
    | otherwise = typ
    where typ = checkList1 symbs exps (BasicType "Any", NoBounds) 

-- |Auxiliary function for the check list function
checkList1 :: [Symbol] -> [Expression] -> (Type, Cardinality) -> Either TypeCheckError (Type, Cardinality)
checkList1 _ [] typ = Right typ
checkList1 symbs (ex : exps) typ 
    | isRight exTyp = exTyp
    | isRight match = match
    | otherwise = checkList1 symbs exps (fromRightUnsafe match)
    where 
        exTyp = checkExpression symbs ex 
        match = typeMatch typ (fromRightUnsafe exTyp)

-- |Checks whether the function that is called is already defined with the same argument types
checkFunctionCall :: [Symbol] -> String -> [Either TypeCheckError (Type, Cardinality)] -> Either TypeCheckError (Type, Cardinality)
checkFunctionCall [] fun args = Left $ UndefinedFunction $ "Undefined function: \"" ++ fun ++ "\" [" ++ concatMap (typeName . fst) (rights args) ++ "]"
checkFunctionCall ((Func n a r):symbolMap) name args
    | length right /= length args = Left $ ErrorInsideFunction (name ++ ": " ++ show args ++ show (lefts args))
    | name == n && all isRight (zipWith typeMatch a right) = Right r
    | otherwise = checkFunctionCall symbolMap name args
    where right = rights args
checkFunctionCall (_:symbolMap) name args = checkFunctionCall symbolMap name args 

--Try to match 2nd type to first type
-- |Checks whether two types are compatible
typeMatch :: (Type, Cardinality) -> (Type, Cardinality) -> Either TypeCheckError (Type, Cardinality)
typeMatch (BasicType "Any", card1) (x, card2) 
    | isRight card = Right (x, fromRightUnsafe card)
    | otherwise = Left $ fromLeftUnsafe card
    where card = cardinalityIncluded card2 card1
typeMatch (BasicType "Double", card1) (BasicType "Integer", card2) 
    | isRight card = Right (BasicType "Dobule", fromRightUnsafe card)
    | otherwise = Left $ fromLeftUnsafe card
    where card = cardinalityIncluded card2 card1
--typeMatch (s, card1) (BasicType s2, card2)
--    | s == BasicType s2 = case cardinalityIncluded card1 card2 of
--        Right card -> Right (s, card)
--        Left err -> Left err 
--    | otherwise = Left $ TypeMismatch (typeName s) s2
typeMatch (s, card1) (s2, card2)
    | s == s2 = case cardinalityIncluded card2 card1 of
        Right card -> Right (s, card)
        Left err -> Left err
    | isJust $ superType s2 = typeMatch (s, card1) (fromJust $ superType s2, card2)
    | otherwise = Left $ TypeMismatch (typeName s) (typeName s2)
    
-- |Checks whether the first cardinality is included into the second one and returns the most restricted cardinality
cardinalityIncluded :: Cardinality -> Cardinality -> Either TypeCheckError Cardinality
cardinalityIncluded x NoBounds = Right x
cardinalityIncluded NoBounds x = Left $ CardinalityMismatch NoBounds x
cardinalityIncluded (OneBound x) (OneBound y)
    | x >= y = Right $ OneBound x
    | otherwise = Left $ CardinalityMismatch (OneBound x) (OneBound y)
cardinalityIncluded (Bounds (x1, x2)) (OneBound y)
    | x1 >= y = Right $ Bounds (x1, x2)
    | otherwise = Left $ CardinalityMismatch (Bounds (x1, x2)) (OneBound y)
cardinalityIncluded (OneBound x) (Bounds (y1, y2)) = Left $ CardinalityMismatch (OneBound x) (Bounds (y1, y2))
cardinalityIncluded (Bounds (x1, x2)) (Bounds (y1, y2))
    | x1 >= y1 && x2 <= y2 = Right $ Bounds (x1, x2)
    | otherwise = Left $ CardinalityMismatch (Bounds (x1, x2)) (Bounds (y1, y2))

-- |Looks in the symbol map for the type of a variable
findVarType :: String -> [Symbol] -> Either TypeCheckError (Type, Cardinality)
findVarType var [] = Left $ UndefinedVariable var
findVarType x ((Var name typ crd):symbols) 
    | x == name = Right (typ, crd)
    | otherwise = findVarType x symbols
findVarType x (_:symbols) = findVarType x symbols