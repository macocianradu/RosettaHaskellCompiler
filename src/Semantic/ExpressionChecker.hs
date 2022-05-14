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
  Func "single exists" [(BasicType "Any", OneBound 0)] (BasicType "Boolean", Bounds (1, 1)),
  Func "multiple exists" [(BasicType "Any", OneBound 0)] (BasicType "Boolean", Bounds (1, 1)),
  Func "contains" [(BasicType "Any", OneBound 0), (BasicType "Any", OneBound 0)] (BasicType "Boolean", Bounds (1, 1)),
  Func "disjoint" [(BasicType "Any", OneBound 0), (BasicType "Any", OneBound 0)] (BasicType "Boolean", Bounds (1, 1)),

  Func "=" [(BasicType "Any", OneBound 0), (BasicType "Any", OneBound 0)] (BasicType "Boolean", Bounds(1, 1)),
  Func ">=" [(BasicType "Any", OneBound 0), (BasicType "Any", OneBound 0)] (BasicType "Boolean", Bounds(1, 1)),
  Func "<=" [(BasicType "Any", OneBound 0), (BasicType "Any", OneBound 0)] (BasicType "Boolean", Bounds(1, 1)),
  Func "<>" [(BasicType "Any", OneBound 0), (BasicType "Any", OneBound 0)] (BasicType "Boolean", Bounds(1, 1)),
  Func ">" [(BasicType "Any", OneBound 0), (BasicType "Any", OneBound 0)] (BasicType "Boolean", Bounds(1, 1)),
  Func "<" [(BasicType "Any", OneBound 0), (BasicType "Any", OneBound 0)] (BasicType "Boolean", Bounds(1, 1)),
  Func "all =" [(BasicType "Any", OneBound 0), (BasicType "Any", Bounds(1, 1))] (BasicType "Boolean", Bounds(1, 1)),
  Func "all <>" [(BasicType "Any", OneBound 0), (BasicType "Any", Bounds(1, 1))] (BasicType "Boolean", Bounds(1, 1)),
  Func "any =" [(BasicType "Any", OneBound 0), (BasicType "Any", Bounds(1, 1))] (BasicType "Boolean", Bounds(1, 1)),
  Func "any <>" [(BasicType "Any", OneBound 0), (BasicType "Any", Bounds(1, 1))] (BasicType "Boolean", Bounds(1, 1)),

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

  Func "count" [(BasicType "Any", OneBound 0)] (BasicType "Integer", Bounds (1, 1))
  ]

-- |Checks whether a function is valid (inputs, outputs are of valid type and all variables are defined) and adds it to the symbol table
addFunction :: ([Type], [Symbol]) -> Function -> Either [TypeCheckError] [Symbol]
addFunction (definedTypes, definedSymbols) (MakeFunction (MakeFunctionSignature name _ inps out) _) =
    case head $ checkAttributes definedTypes [out] of
        Left err -> Left [err]
        Right checkedOutput -> if null (lefts checkedInputs)
            then if name `elem` map funcName definedSymbols
                then Left [MultipleDeclarations name]
                else Right $ Func name (map typeAndCardinality (rights checkedInputs)) (attributeType checkedOutput, Model.Type.cardinality out) : definedSymbols
            else Left $ lefts checkedInputs
    where
        checkedInputs = checkAttributes definedTypes inps

-- |Adds a newly defined variable to the symbol table
addVariables :: [Symbol] -> [TypeAttribute] -> [Symbol]
addVariables s [] = s
addVariables s ((MakeTypeAttribute name typ crd _)  : vars) = Var name (toHaskell typ) crd : addVariables s vars

-- |Checks the type of a given expression
checkExpression :: [Symbol] -> Expression -> Either TypeCheckError ExplicitExpression
--checkExpression sym _ = error $ show sym
checkExpression symbolMap (Variable var) = findVarType var symbolMap
checkExpression _ (Int val) = Right $ Value val $ MakeCoercion [MakeIdCoercion (BasicType "Integer")] (MakeCardinalityIdCoercion (Bounds (1, 1)))
checkExpression _ (Real val) = Right $ Value val $ MakeCoercion [MakeIdCoercion (BasicType "Double")] (MakeCardinalityIdCoercion (Bounds (1, 1)))
checkExpression _ (Boolean val) = Right $ Value val $ MakeCoercion [MakeIdCoercion (BasicType "Boolean")] (MakeCardinalityIdCoercion (Bounds (1, 1)))
checkExpression _ Empty = Right $ Value "empty" $ MakeCoercion [MakeIdCoercion (BasicType "Empty")] (MakeCardinalityIdCoercion (Bounds (0, 0)))
checkExpression symbolMap (PathExpression ex1 (Variable b)) =
    case checkExpression symbolMap ex1 of
        Left err -> Left err
        Right exp1 -> case findAttributeType b (typeAttributes type1) of
            Left err -> Left $ UndefinedVariable $ show type1--ex1 ++ " -> " ++ b
            Right exp2 -> Right $ ExplicitPath exp1 exp2 (returnCoercion exp2)
            where
                type1 = coercionType $ typeCoercion $ returnCoercion exp1
--checkExpression symbolMap (PathExpression ex1 (PathExpression ))
checkExpression symbolMap (Parens ex) =
    case checkExpression symbolMap ex of
        Left err -> Left err
        Right exp -> Right $ ExplicitParens exp
-- |Getting here means that an expression is used inside a path expression and this is not supported
checkExpression _ (PathExpression _ ex) = Left $ UnsupportedExpressionInPathExpression $ show ex
checkExpression symbolMap (List lst) = checkList symbolMap lst
checkExpression symbolMap (PrefixExp name ex) = checkFunctionCall symbolMap name [checkExpression symbolMap ex]
checkExpression symbolMap (Function name exps) = checkFunctionCall symbolMap name (map (checkExpression symbolMap) exps)
checkExpression symbolMap (PostfixExp name ex) = checkFunctionCall symbolMap name [checkExpression symbolMap ex]
checkExpression symbolMap (InfixExp name ex1 ex2) = checkFunctionCall symbolMap name (checkExpression symbolMap ex1: [checkExpression symbolMap ex2])
-- |Checks if the condition of an if expression is of type boolean, and then checks the expression of the then statement
checkExpression symbolMap (IfSimple cond ex) =
    case checkExpression symbolMap cond of
        Left err -> Left $ IfConditionNotBoolean $ show err
        Right condType -> case returnCoercion condType `coercionIncluded` MakeCoercion [MakeIdCoercion (BasicType "Boolean")] (MakeCardinalityIdCoercion (Bounds (1, 1))) of
            Left err -> Left $ IfConditionNotBoolean $ show cond ++ " :: " ++ show (coercionType $ typeCoercion $ returnCoercion condType)
            Right condCoerce ->
                case checkExpression symbolMap ex of
                    Left err -> Left err
                    Right thenExp ->
                        Right $ ExplicitIfSimple (condType, condCoerce)
                        (thenExp, thenCoercion)
                        (MakeCoercion [MakeIdCoercion $ coercionType $ typeCoercion $ returnCoercion thenExp]
                            (MakeCardinalityIdCoercion $ smallestBound (Bounds (0, 0)) (toCardinality $ cardinalityCoercion $ returnCoercion thenExp)))
                        where
                            thenCoercion = MakeCoercion [MakeIdCoercion $ coercionType $ typeCoercion $ returnCoercion thenExp]
                                (MakeCardinalityIdCoercion $ toCardinality $ cardinalityCoercion $ returnCoercion thenExp)

-- |Checks if the condition of the if statement is of type boolean, and then checks that both the then and else statements have the same type
checkExpression symbolMap (IfElse cond ex1 ex2) =
    case checkExpression symbolMap cond of
        Left err -> Left $ IfConditionNotBoolean $ show err
        Right condType -> case returnCoercion condType `coercionIncluded` MakeCoercion [MakeIdCoercion (BasicType "Boolean")] (MakeCardinalityIdCoercion (Bounds (1, 1))) of
            Left err -> Left $ IfConditionNotBoolean $ show cond ++ " :: " ++ show (coercionType $ typeCoercion $ returnCoercion condType)
            Right condCoerce ->
                case checkExpression symbolMap ex1 of
                    Left err -> Left $ ErrorInsideFunction $ show err
                    Right thenExp -> case checkExpression symbolMap ex2 of
                        Left err -> Left $ ErrorInsideFunction $ show err
                        Right elseExp ->
                            Right $ ExplicitIfElse (condType, condCoerce)
                            (thenExp, MakeCoercion [MakeIdCoercion $ coercionType $ typeCoercion $ returnCoercion thenExp]
                             (MakeCardinalityIdCoercion $ toCardinality $ cardinalityCoercion $ returnCoercion thenExp))
                            (elseExp, MakeCoercion [MakeIdCoercion $ coercionType $ typeCoercion $ returnCoercion elseExp]
                             (MakeCardinalityIdCoercion $ toCardinality $ cardinalityCoercion $ returnCoercion elseExp)) (returnCoercion thenExp)
                        --(typeMatch ex1Type ex2Type, smallestBound ex1Card ex2Type)

-- |TODO Handle nested lists and lists with parens
-- |Checks that all the expressions in a list have compatible types
checkList :: [Symbol] -> [Expression] -> Either TypeCheckError ExplicitExpression
checkList _ [] = Right $ ExplicitList [ExplicitEmpty]
checkList symbs (ex : exps) =
    case checkExpression symbs ex of
        Left err -> Left err
        Right x ->
            case checkList1 symbs exps (coercionType $ typeCoercion $ returnCoercion x, toCardinality $ cardinalityCoercion $ returnCoercion x) of
                Left err -> Left err
                Right exp -> Right $ ExplicitList exp

-- |Auxiliary function for the check list function
checkList1 :: [Symbol] -> [Expression] -> (Type, Cardinality) -> Either TypeCheckError [ExplicitExpression]
checkList1 _ [] typ = Right [ExplicitEmpty]
checkList1 symbs (ex : exps) typ =
    case checkExpression symbs ex of
        Left err -> Left err
        Right exCo ->
            case fst typ `isSubType` exTyp of
                Left err -> Left err
                Right _ ->
                    case checkList1 symbs exps (exTyp, smallestBound exCard (snd typ)) of
                        Left err -> Left err
                        Right explicitEx -> Right [ExplicitList explicitEx]
                where
                    exTyp = coercionType $ typeCoercion $ returnCoercion exCo
                    exCard = toCardinality $ cardinalityCoercion $ returnCoercion exCo

-- |Checks whether the function that is called is already defined with the same argument types
checkFunctionCall :: [Symbol] -> String -> [Either TypeCheckError ExplicitExpression ] -> Either TypeCheckError ExplicitExpression
checkFunctionCall [] fun args = Left $ UndefinedFunction $ "Undefined function: \"" ++ fun ++ "\" [" ++ show (rights args) ++ "]"
checkFunctionCall ((Func n a r):symbolMap) name args
    | length (rights args) /= length args = Left $ ErrorInsideFunction (name ++ ": " ++ show args ++ show (lefts args))
    | name == n && all isRight coerce = Right $ ExplicitFunction name (zip (rights args) (rights coerce)) (MakeCoercion [MakeIdCoercion (fst r)] (MakeCardinalityIdCoercion (snd r)))
    | otherwise = checkFunctionCall symbolMap name args
    where
        argCoerce = map returnCoercion (rights args)
        coerce = zipWith coercionIncluded argCoerce (map createCoercion a)
checkFunctionCall (_:symbolMap) name args = checkFunctionCall symbolMap name args

typeIncluded :: (Type, Cardinality) -> (Type, Cardinality) -> Either TypeCheckError Coercion
typeIncluded (t1, c1) (t2, c2) =
    case t1 `isSubType` t2 of
        Left err -> Left err
        Right typeCoercion ->
            case c1 `cardinalityIncluded` c2 of
                Left err -> Left err
                Right cardCoercion -> Right $ MakeCoercion typeCoercion cardCoercion

coercionIncluded :: Coercion -> Coercion -> Either TypeCheckError Coercion
coercionIncluded c1 c2 = (coercionType (typeCoercion c1), coercionCardinality [cardinalityCoercion c1]) `typeIncluded` (coercionType (typeCoercion c2), coercionCardinality [cardinalityCoercion c2])

-- |Finds the most specific super type of the two types
typeMatch :: Type -> Type -> Type
-- |Any matches with any type
typeMatch (BasicType "Any") x = x
typeMatch x (BasicType "Any") = x
-- |Integer can be a double
-- typeMatch (BasicType "Integer") (BasicType "Double") = BasicType "Double"
-- typeMatch (BasicType "Double") (BasicType "Integer") = BasicType "Double"
typeMatch x (BasicType y) =
    case x `isSubType` BasicType y of
        Left err -> BasicType "Object"
        Right _ -> BasicType y
-- |First check x with all the supertypes of y, then go higher on the supertypes of x and repeat
typeMatch x y = case x `isSubType` y of
        Left err -> typeMatch x (superType y)
        Right _ -> y

-- |Looks in the symbol map for the type of a variable
findVarType :: String -> [Symbol] -> Either TypeCheckError ExplicitExpression
findVarType var [] = Left $ UndefinedVariable var
findVarType x ((Var name typ crd):symbols)
    | x == name = Right $ ExplicitVariable x (MakeCoercion [MakeIdCoercion typ] (MakeCardinalityIdCoercion crd))
    | otherwise = findVarType x symbols
findVarType x (_:symbols) = findVarType x symbols

-- |Find whether there is a attribute with the given name in the given type, and returns the attribute's type
findAttributeType :: String -> [TypeAttribute] -> Either TypeCheckError ExplicitExpression
findAttributeType var [] = Left $ UndefinedVariable var
findAttributeType var (t : ts)
    | var == attributeName t = Right $ ExplicitVariable var (MakeCoercion [MakeIdCoercion $ attributeType t] (MakeCardinalityIdCoercion $ Model.Type.cardinality t))
    | otherwise = findAttributeType var ts

-- |Checks whether the first argument is a subtype of the second argument
isSubType :: Type -> Type -> Either TypeCheckError [TypeCoercion]
isSubType (BasicType "Integer") (BasicType "Double") = Right [MakeTypeCoercion (BasicType "Integer") (BasicType "Double") "fromInteger"]
isSubType (BasicType x) y
    | x == typeName y = Right [MakeIdCoercion y]
    | otherwise = Left $ TypeMismatch x (typeName y)
isSubType x y
    | typeName x == typeName y = Right [MakeIdCoercion x]
    | otherwise = case isSubType (superType x) y of
        Left e -> Left e
        Right transforms -> Right $ MakeSuperCoercion x y : transforms

-- |Checks whether the first cardinality is included into the second one 
cardinalityIncluded :: Cardinality -> Cardinality -> Either TypeCheckError CardinalityCoercion
-- |Special Cases
-- |Transform nothing into a maybe
cardinalityIncluded (Bounds (0, 0)) (Bounds (0, 1)) = Right $ MakeNothing2MaybeCoercion (Bounds (0, 0)) (Bounds (0, 1))
-- |Transform nothing into a list
cardinalityIncluded (Bounds (0, 0)) (OneBound 0) = Right $ MakeNothing2ListCoercion (Bounds (0, 0)) (OneBound 0)
-- |Transform maybe into list
cardinalityIncluded (Bounds (0, 1)) (OneBound 0) = Right $ MakeMaybe2ListCoercion (Bounds (0, 1)) (OneBound 0)
-- |Transform object into maybe
cardinalityIncluded (Bounds (1, 1)) (Bounds (0, 1)) = Right $ MakeObject2MaybeCoercion (Bounds (0, 1)) (Bounds (0, 1))
-- |Transform object into list
cardinalityIncluded (Bounds (1, 1)) (OneBound 1) = Right $ MakeObject2ListCoercion (Bounds (0, 1)) (OneBound 1)
cardinalityIncluded (Bounds (1, 1)) (OneBound 0) = Right $ MakeObject2ListCoercion (Bounds (0, 1)) (OneBound 1)
-- |General
cardinalityIncluded (OneBound x) (OneBound y)
    | x >= y = Right $ MakeListCardinalityCoercion (OneBound x) (OneBound y)
    | otherwise = Left $ CardinalityMismatch (OneBound x) (OneBound y)
cardinalityIncluded (Bounds (x1, y1)) (OneBound y)
    | x1 >= y = Right $ MakeListCardinalityCoercion (Bounds (x1, y1)) (OneBound y)
    | otherwise = Left $ CardinalityMismatch (Bounds (x1, y1)) (OneBound y)
cardinalityIncluded (OneBound x) (Bounds (x2, y2)) = Left $ CardinalityMismatch (OneBound x) (Bounds (x2, y2))
cardinalityIncluded (Bounds (x1, x2)) (Bounds (y1, y2))
    | x1 >= y1 && x2 <= y2 = Right $ MakeListCardinalityCoercion (Bounds (x1, x2)) (Bounds (y1, y2))
    | otherwise = Left $ CardinalityMismatch (Bounds (x1, x2)) (Bounds (y1, y2))
