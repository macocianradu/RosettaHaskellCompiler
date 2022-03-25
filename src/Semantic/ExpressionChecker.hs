module Semantic.ExpressionChecker where

import Model.Function
import Data.Either
import Data.Maybe
import Model.Type
import Semantic.TypeChecker
import Utils.Utils
  
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

instance Eq Symbol where
    (==) (Var name1 _ _) (Var name2 _ _)
        | name1 == name2 = True
        | otherwise = False
    (==) (Func name1 _ _) (Func name2 _ _)
        | name1 == name2 = True
        | otherwise = False
    (==) _ _ = False
  
  
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
    | null (lefts checkedInputs) && isRight checkedOutput = if name `elem` map funcName definedSymbols
        then Left [MultipleDeclarations name]
        else Right $ Func name (map typeAndCardinality (rights checkedInputs)) (attributeType $ fromRightUnsafe checkedOutput, Model.Type.cardinality out) : definedSymbols
    | isLeft checkedOutput = Left [fromLeftUnsafe checkedOutput]
    | otherwise = Left $  lefts checkedInputs 
    where 
        checkedInputs = checkAttributes definedTypes inps
        checkedOutput = head $ checkAttributes definedTypes [out]
        
-- |Adds a newly defined variable to the symbol table
addVariables :: [Symbol] -> [TypeAttribute] -> [Symbol]
addVariables s [] = s
addVariables s ((MakeTypeAttribute name typ crd _)  : vars) = Var name (toHaskell typ) crd : addVariables s vars
        
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
    | isRight condType && isSubType (fst $ fromRightUnsafe condType) (BasicType "Boolean") && snd (fromRightUnsafe condType) == Bounds (1, 1) = 
        case checkedExp of
            Right (typ, Bounds(_, x)) -> Right (typ, Bounds(0, x))
            Right x -> Right x
            Left err -> Left err
    | otherwise = Left $ IfConditionNotBoolean $ show condType
    where 
        condType = checkExpression symbolMap cond
        checkedExp = checkExpression symbolMap ex
-- |Checks if the condition of the if statement is of type boolean, and then checks that both the then and else statements have the same type
checkExpression symbolMap (IfElse cond ex1 ex2)
    | isLeft condType || not (isSubType (fst $ fromRightUnsafe condType) (BasicType "Boolean") && snd (fromRightUnsafe condType) == Bounds (1, 1)) = Left $ IfConditionNotBoolean $ show cond ++ " :: " ++ show condType
    | otherwise = case checkExpression symbolMap ex1 of
        Left err -> Left $ ErrorInsideFunction $ show err
        Right ex1Type -> case checkExpression symbolMap ex2 of
            Left err -> Left $ ErrorInsideFunction $ show err
            Right ex2Type -> Right (typeMatch (fst ex1Type) (fst ex2Type), smallestBound (snd ex1Type) (snd ex2Type))
    where   condType = checkExpression symbolMap cond

-- |Checks that all the expressions in a list have compatible types
checkList :: [Symbol] -> [Expression] -> Either TypeCheckError (Type, Cardinality)
checkList _ [] = Right (BasicType "Empty", Bounds(0, 0))
checkList symbs (ex : exps)
    | isRight typ = checkList1 symbs exps (fromRightUnsafe typ) 
    | otherwise = typ
    where typ = checkExpression symbs ex
    
-- |Auxiliary function for the check list function
checkList1 :: [Symbol] -> [Expression] -> (Type, Cardinality) -> Either TypeCheckError (Type, Cardinality)
checkList1 _ [] typ = Right typ
checkList1 symbs (ex : exps) typ 
    | isLeft exTyp = exTyp
    | sub = checkList1 symbs exps (fst typ, smallestBound (snd $ fromRightUnsafe exTyp) (snd typ))
    | sup = checkList1 symbs exps (fst $ fromRightUnsafe exTyp, smallestBound (snd $ fromRightUnsafe exTyp) (snd typ))
    | otherwise = Left $ TypeMismatch (typeName $ fst typ) (typeName $ fst (fromRightUnsafe exTyp))
    where 
        exTyp = checkExpression symbs ex 
        sub = isSubType (fst typ) (fst (fromRightUnsafe exTyp))
        sup = isSubType (fst (fromRightUnsafe  exTyp)) (fst typ)

-- |Checks whether the function that is called is already defined with the same argument types
checkFunctionCall :: [Symbol] -> String -> [Either TypeCheckError (Type, Cardinality)] -> Either TypeCheckError (Type, Cardinality)
checkFunctionCall [] fun args = Left $ UndefinedFunction $ "Undefined function: \"" ++ fun ++ "\" [" ++ show (rights args) ++ "]"
checkFunctionCall ((Func n a r):symbolMap) name args
    | length right /= length args = Left $ ErrorInsideFunction (name ++ ": " ++ show args ++ show (lefts args))
    | name == n && all isRight (zipWith typeIncluded right a) = Right r
    | otherwise = checkFunctionCall symbolMap name args
    where 
        right = rights args
checkFunctionCall (_:symbolMap) name args = checkFunctionCall symbolMap name args 

typeIncluded :: (Type, Cardinality) -> (Type, Cardinality) -> Either TypeCheckError Bool
typeIncluded (t1, c1) (t2, c2)
    | t1 `isSubType` t2 && cardinalityIncluded c1 c2 = Right True
    | t1 `isSubType` t2 = Left $ CardinalityMismatch c1 c2
    | otherwise = Left $ TypeMismatch (typeName t1) (typeName t2)

-- |Finds the most specific super type of the two types
typeMatch :: Type -> Type -> Type
-- |Any matches with any type
typeMatch (BasicType "Any") x = x
typeMatch x (BasicType "Any") = x
-- |Integer can be a double
-- typeMatch (BasicType "Integer") (BasicType "Double") = BasicType "Double"
-- typeMatch (BasicType "Double") (BasicType "Integer") = BasicType "Double"
typeMatch x (BasicType y) 
    | x `isSubType` BasicType y = x
    | otherwise = BasicType "Object"
-- |First check x with all the supertypes of y, then go higher on the supertypes of x and repeat
typeMatch x y
    | x `isSubType` y = x
    | otherwise = typeMatch x (superType y)

-- |Looks in the symbol map for the type of a variable
findVarType :: String -> [Symbol] -> Either TypeCheckError (Type, Cardinality)
findVarType var [] = Left $ UndefinedVariable var
findVarType x ((Var name typ crd):symbols)
    | x == name = Right (typ, crd)
    | otherwise = findVarType x symbols
findVarType x (_:symbols) = findVarType x symbols