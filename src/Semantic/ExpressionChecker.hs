module Semantic.ExpressionChecker where

import Model.Function
import Data.Either
import Data.Maybe
import Model.Type
import Semantic.TypeChecker
import Utils.Utils

-- |A declared variable or function
data Symbol = Var{
   symbolName :: String,
   declaredType :: Type,
   cardinality :: Cardinality
   }
   | Func {
   symbolName :: String,
   argsType :: [(Type, Cardinality)],
   returnType :: (Type, Cardinality)
   }

instance Show Symbol where
    show (Var n t c) = "Variable {name: " ++ show n ++ ", type: " ++  show t ++ ", card: " ++ show c ++ "}"
    show (Func n i o) = "Function {name: " ++ show n ++ ", arguments: " ++  show [((typeName . fst) t, (snd t)) | t <- i]  ++ ", return: (" ++ typeName (fst o) ++ ", " ++ show (snd o) ++ ")}"

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
  Func "exists" [(BasicType "Any", OneBound 0)] (BasicType "Boolean", Bounds(1, 1)),
  Func "is absent" [(BasicType "Any", Bounds (0, 1))] (BasicType "Boolean", Bounds (1, 1)),
  Func "single exists" [(BasicType "Any", OneBound 0)] (BasicType "Boolean", Bounds (1, 1)),
  Func "multiple exists" [(BasicType "Any", OneBound 0)] (BasicType "Boolean", Bounds (1, 1)),
  Func "only exists" [(BasicType "Any", OneBound 0)] (BasicType "Boolean", Bounds (1, 1)),

  Func "=" [(BasicType "Any", Bounds(1, 1)), (BasicType "Any", Bounds(1, 1))] (BasicType "Boolean", Bounds(1, 1)),
  Func "=" [(BasicType "Any", Bounds(0, 1)), (BasicType "Any", Bounds(0, 1))] (BasicType "Boolean", Bounds(1, 1)),
  Func "=" [(BasicType "Any", OneBound 0), (BasicType "Any", OneBound 0)] (BasicType "Boolean", Bounds(1, 1)),
  Func ">=" [(BasicType "Any", Bounds(1, 1)), (BasicType "Any", Bounds(1, 1))] (BasicType "Boolean", Bounds(1, 1)),
  Func ">=" [(BasicType "Any", Bounds(0, 1)), (BasicType "Any", Bounds(0, 1))] (BasicType "Boolean", Bounds(1, 1)),
  Func ">=" [(BasicType "Any", OneBound 0), (BasicType "Any", OneBound 0)] (BasicType "Boolean", Bounds(1, 1)),
  Func "<=" [(BasicType "Any", Bounds(1, 1)), (BasicType "Any", Bounds(1, 1))] (BasicType "Boolean", Bounds(1, 1)),
  Func "<=" [(BasicType "Any", Bounds(0, 1)), (BasicType "Any", Bounds(0, 1))] (BasicType "Boolean", Bounds(1, 1)),
  Func "<=" [(BasicType "Any", OneBound 0), (BasicType "Any", OneBound 0)] (BasicType "Boolean", Bounds(1, 1)),
  Func "<>" [(BasicType "Any", Bounds(1, 1)), (BasicType "Any", Bounds(1, 1))] (BasicType "Boolean", Bounds(1, 1)),
  Func "<>" [(BasicType "Any", Bounds(0, 1)), (BasicType "Any", Bounds(0, 1))] (BasicType "Boolean", Bounds(1, 1)),
  Func "<>" [(BasicType "Any", OneBound 0), (BasicType "Any", OneBound 0)] (BasicType "Boolean", Bounds(1, 1)),
  Func ">" [(BasicType "Any", Bounds(1, 1)), (BasicType "Any", Bounds(1, 1))] (BasicType "Boolean", Bounds(1, 1)),
  Func ">" [(BasicType "Any", Bounds(0, 1)), (BasicType "Any", Bounds(0, 1))] (BasicType "Boolean", Bounds(1, 1)),
  Func ">" [(BasicType "Any", OneBound 0), (BasicType "Any", OneBound 0)] (BasicType "Boolean", Bounds(1, 1)),
  Func "<" [(BasicType "Any", Bounds(1, 1)), (BasicType "Any", Bounds(1, 1))] (BasicType "Boolean", Bounds(1, 1)),
  Func "<" [(BasicType "Any", Bounds(0, 1)), (BasicType "Any", Bounds(0, 1))] (BasicType "Boolean", Bounds(1, 1)),
  Func "<" [(BasicType "Any", OneBound 0), (BasicType "Any", OneBound 0)] (BasicType "Boolean", Bounds(1, 1)),

  Func "+" [(BasicType "Integer", Bounds (1, 1)), (BasicType "Integer", Bounds (1, 1))] (BasicType "Integer", Bounds (1, 1)),
  Func "+" [(BasicType "Double", Bounds (1, 1)), (BasicType "Double", Bounds (1, 1))] (BasicType "Double", Bounds (1, 1)),
  Func "+" [(BasicType "Integer", OneBound 0), (BasicType "Integer", OneBound 0)] (BasicType "Integer", Bounds (1, 1)),
  Func "+" [(BasicType "Double", OneBound 0), (BasicType "Double", OneBound 0)] (BasicType "Double", Bounds (1, 1)),
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

-- |A list of the allowed list functions, the type of their expression and their return type
listFunctionTypes :: Coercion -> Coercion -> [(String, Coercion, Coercion)]
listFunctionTypes inp ex = [
    -- The function given to a filter must be boolean and it can return anything
    ("filter", MakeCoercion [MakeIdCoercion (BasicType "Boolean")] (MakeCardinalityIdCoercion (OneBound 0)), inp),
    ("map", MakeCoercion [MakeIdCoercion (BasicType "Any")] (MakeCardinalityIdCoercion (OneBound 0)), MakeCoercion (typeCoercion ex) (cardinalityCoercion inp)),
    ("reduce", MakeCoercion [MakeIdCoercion (BasicType "Any")] (MakeCardinalityIdCoercion (Bounds (1, 1))), ex)
    ]

listUnaryFunctionTypes :: Coercion -> [(String, Coercion)]
listUnaryFunctionTypes inp = [
    ("only-element", MakeCoercion (typeCoercion inp) (MakeCardinalityIdCoercion (Bounds (1, 1)))),
     ("flatten", inp)
    ]

-- |Checks whether a function is valid (inputs, outputs are of valid type and all variables are defined) and adds it to the symbol table
addFunction :: ([Type], [Symbol]) -> Function -> Either [TypeCheckError] [Symbol]
addFunction (definedTypes, definedSymbols) (MakeFunction (MakeFunctionSignature name _ inps out) _ _) =
    case head $ checkAttributes definedTypes [out] of
        Left err -> Left [err]
        Right checkedOutput -> if null (lefts checkedInputs)
            then if name `elem` map symbolName definedSymbols
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
checkExpression :: [Type] -> [Symbol] -> Expression -> Either TypeCheckError ExplicitExpression
--checkExpression sym _ = error $ show sym
checkExpression _ symbolMap (Variable var) = findVarType var symbolMap
checkExpression _ _ (Int val) = Right $ Value val $ MakeCoercion [MakeIdCoercion (BasicType "Integer")] (MakeCardinalityIdCoercion (Bounds (1, 1)))
checkExpression _ _ (Real val) = Right $ Value val $ MakeCoercion [MakeIdCoercion (BasicType "Double")] (MakeCardinalityIdCoercion (Bounds (1, 1)))
checkExpression _ _ (Boolean val) = Right $ Value val $ MakeCoercion [MakeIdCoercion (BasicType "Boolean")] (MakeCardinalityIdCoercion (Bounds (1, 1)))
checkExpression _ _ Empty = Right $ Value "empty" $ MakeCoercion [MakeIdCoercion (BasicType "Empty")] (MakeCardinalityIdCoercion (Bounds (0, 0)))
checkExpression defT symbolMap (Enum enum val) = case getType enum  defT of 
    Left err -> Left err
    Right typ -> if val `elem` map attributeName (typeAttributes typ) 
        then Right $ ExplicitEnumCall enum val $ MakeCoercion [MakeIdCoercion typ] (MakeCardinalityIdCoercion (Bounds (1, 1)))  
        else Left $ UndefinedVariable val
checkExpression defT symbolMap (Reduce op lst v1 v2 cond) =
    case checkExpression defT symbolMap lst of
        Left err -> Left $ ErrorInsideFunction $ op ++ ": " ++ show err
        Right checkedLst -> let it = getNextItem symbolMap in
            case checkExpression defT 
                (addVariables symbolMap [MakeTypeAttribute it (typeFromExpression checkedLst) (Bounds (1,1)) Nothing, 
                                                                MakeTypeAttribute v1 (typeFromExpression checkedLst) (Bounds (1,1)) Nothing,
                                                                MakeTypeAttribute v2 (typeFromExpression checkedLst) (Bounds (1,1)) Nothing])
                (replaceVar cond it) of
                    Left err -> Left $ ErrorInsideFunction $ op ++ ": " ++ show err
                    Right condType -> case returnCoercion condType `coercionIncluded` head [snd3 x | x <- listOps, fst3 x == op] of
                        Left err -> Left $ ErrorInsideFunction $ op ++ ": " ++ show err
                        Right checkedCond -> Right $ ExplicitReduce op checkedLst v1 v2 (changeCoercion condType checkedCond) (head [trd3 x | x <- listOps, fst3 x == op])
                        where
                            listOps = listFunctionTypes (returnCoercion checkedLst) (returnCoercion condType)

checkExpression defT symbolMap (ListOp op lst cond) =
    case checkExpression defT symbolMap lst of
        Left err -> Left $ ErrorInsideFunction $ op ++ ": " ++ show err
        Right checkedLst -> {-if toCardinality (cardinalityCoercion (returnCoercion checkedLst)) == Bounds(1, 1) 
            then Left $ ListOperationNotOnList $ show op ++ ": " ++ show lst
            else -}let it = getNextItem symbolMap in
                case checkExpression defT 
                    (addVariables symbolMap [MakeTypeAttribute it (typeFromExpression checkedLst) (Bounds (1,1)) Nothing]) 
                    (replaceVar cond it) of
                Left err -> Left $ ErrorInsideFunction $ op ++ ": " ++ show err
                Right condType -> case returnCoercion condType `coercionIncluded` head [snd3 x | x <- listOps, fst3 x == op] of
                    Left err -> Left $ ErrorInsideFunction $ op ++ ": " ++ show err
                    Right checkedCond -> Right $ ExplicitListOp op checkedLst (changeCoercion condType checkedCond) (head [trd3 x | x <- listOps, fst3 x == op])
                    where
                        listOps = listFunctionTypes (returnCoercion checkedLst) (returnCoercion condType)
checkExpression defT symbolMap (ListUnaryOp op lst) =
    case checkExpression defT symbolMap lst of
        Left err -> Left $ ErrorInsideFunction $ op ++ ": " ++ show err
        Right checkedLst -> {-if toCardinality (cardinalityCoercion (returnCoercion checkedLst)) == Bounds(1, 1) 
            then Left $ ListOperationNotOnList $ show op ++ ": " ++ show lst
            else -}if op `elem` map fst (listUnaryFunctionTypes (returnCoercion checkedLst))
                then Right $ ExplicitListUnaryOp op checkedLst (head [snd x | x <- listUnaryFunctionTypes (returnCoercion checkedLst), fst x == op])
                else Left $ UndefinedFunction op
checkExpression _ _ (Keyword k) = Right $ ExplicitKeyword k
checkExpression defT symbolMap (PathExpression ex1 (Variable b)) = 
    case checkExpression defT symbolMap ex1 of
        Left err -> Left err
        Right exp1 -> case findAttributeTypeRec defT b type1 of
            Left err -> Left $ UndefinedVariable $ show (typeName type1) ++ " -> " ++ b
            Right exp2 -> case cardC of
                    Left err -> Left $ PathExpressionOnList (show ex1) 
                    Right c -> if MakeCondition Nothing (Keyword "one-of") `elem` conditions (typeFromExpression exp1) 
                        then Right $ ExplicitPath (changeCoercion exp1 ((returnCoercion exp1){cardinalityCoercion = c})) (changeCoercion exp2 exp2C) exp2C
                        else Right $ ExplicitPath (changeCoercion exp1 ((returnCoercion exp1){cardinalityCoercion = c})) exp2 (returnCoercion exp2)
                    where
                        exp2C = MakeCoercion (typeCoercion $ returnCoercion exp2) (MakeOneOfCoercion (Bounds (1,1)))
                        cardC = case crd1 of
                            Bounds (1, 1) -> Right $ MakeCardinalityIdCoercion crd1
                            Bounds (0, 1) -> Right $ MakeMaybe2ObjectCoercion (Bounds (1, 1))
                            _ -> Left $ PathExpressionOnList (show ex1)

            where
                type1 = typeFromExpression exp1
                crd1 = toCardinality $ cardinalityCoercion $ returnCoercion exp1
-- |Getting here means that an expression is used inside a path expression and this is not supported
checkExpression _ _ (PathExpression _ ex) = Left $ UnsupportedExpressionInPathExpression $ show ex
--checkExpression symbolMap (PathExpression ex1 (PathExpression ))
checkExpression defT symbolMap (Parens ex) =
    case checkExpression defT symbolMap ex of
        Left err -> Left err
        Right exp -> Right $ ExplicitParens exp (returnCoercion exp)
checkExpression defT symbolMap (List lst) = checkList defT symbolMap lst
checkExpression defT symbolMap (PrefixExp name ex) = checkFunctionCall symbolMap name [checkExpression defT symbolMap ex]
checkExpression defT symbolMap (Function name exps) = checkFunctionCall symbolMap name (map (checkExpression defT symbolMap) exps)
checkExpression defT symbolMap (PostfixExp name ex) = checkFunctionCall symbolMap name [checkExpression defT symbolMap ex]
checkExpression defT symbolMap (InfixExp name ex1 ex2) = checkFunctionCall symbolMap name (checkExpression defT symbolMap ex1: [checkExpression defT symbolMap ex2])
-- |Checks if the condition of an if expression is of type boolean, and then checks the expression of the then statement
checkExpression defT symbolMap (IfSimple cond ex) =
    case checkExpression defT symbolMap cond of
        Left err -> Left $ IfConditionNotBoolean $ show err
        Right condType -> case returnCoercion condType `coercionIncluded` MakeCoercion [MakeIdCoercion (BasicType "Boolean")] (MakeCardinalityIdCoercion (Bounds (1, 1))) of
            Left err -> Left $ IfConditionNotBoolean $ show cond ++ " :: " ++ show (typeFromExpression condType)
            Right condCoerce ->
                case checkExpression defT symbolMap ex of
                    Left err -> Left err
                    Right thenExp ->
                        Right $ ExplicitIfSimple (condType, condCoerce)
                        (thenExp, thenCoercion)
                        (MakeCoercion [MakeIdCoercion $ typeFromExpression thenExp]
                            (MakeCardinalityIdCoercion $ smallestBound (Bounds (0, 0)) (toCardinality $ cardinalityCoercion $ returnCoercion thenExp)))
                        where
                            thenCoercion = MakeCoercion [MakeIdCoercion $ typeFromExpression thenExp]
                                (MakeCardinalityIdCoercion $ toCardinality $ cardinalityCoercion $ returnCoercion thenExp)

-- |Checks if the condition of the if statement is of type boolean, and then checks that both the then and else statements have the same type
checkExpression defT symbolMap (IfElse cond ex1 ex2) =
    case checkExpression defT symbolMap cond of
        Left err -> Left $ IfConditionNotBoolean $ show err
        Right condType -> case returnCoercion condType `coercionIncluded` MakeCoercion [MakeIdCoercion (BasicType "Boolean")] (MakeCardinalityIdCoercion (Bounds (1, 1))) of
            Left err -> Left $ IfConditionNotBoolean $ show cond ++ " :: " ++ show (typeFromExpression condType)
            Right condCoerce ->
                case checkExpression defT symbolMap ex1 of
                    Left err -> Left $ ErrorInsideFunction $ show err
                    Right thenExp -> case checkExpression defT symbolMap ex2 of
                        Left err -> Left $ ErrorInsideFunction $ show err
                        Right elseExp -> case returnCoercion elseExp `coercionIncluded` returnCoercion thenExp of
                            Left _ -> Left $ IfExpressionsDifferentTypes (show thenExp) (show elseExp)
                            Right c -> Right $ ExplicitIfElse (condType, condCoerce)
                                (thenExp, MakeCoercion [MakeIdCoercion $ typeFromExpression thenExp]
                                (MakeCardinalityIdCoercion $ toCardinality $ cardinalityCoercion $ returnCoercion thenExp))
                                (elseExp, c) (returnCoercion thenExp)
                        --(typeMatch ex1Type ex2Type, smallestBound ex1Card ex2Type)

-- |TODO Handle nested lists and lists with parens
-- |Checks that all the expressions in a list have compatible types
checkList :: [Type] -> [Symbol] -> [Expression] -> Either TypeCheckError ExplicitExpression
checkList _ _ [] = Right $ ExplicitList [ExplicitEmpty]
checkList defT symbs (ex : exps) =
    case checkExpression defT symbs ex of
        Left err -> Left err
        Right x ->
            case checkList1 defT symbs exps (typeFromExpression x, toCardinality $ cardinalityCoercion $ returnCoercion x) of
                Left err -> Left err
                Right exp -> Right $ ExplicitList exp

-- |Auxiliary function for the check list function
checkList1 :: [Type] -> [Symbol] -> [Expression] -> (Type, Cardinality) -> Either TypeCheckError [ExplicitExpression]
checkList1 _ _ [] typ = Right [ExplicitEmpty]
checkList1 defT symbs (ex : exps) typ =
    case checkExpression defT symbs ex of
        Left err -> Left err
        Right exCo ->
            case fst typ `isSubType` exTyp of
                Left err -> Left err
                Right _ ->
                    case checkList1 defT symbs exps (exTyp, smallestBound exCard (snd typ)) of
                        Left err -> Left err
                        Right explicitEx -> Right [ExplicitList explicitEx]
                where
                    exTyp = typeFromExpression exCo
                    exCard = toCardinality $ cardinalityCoercion $ returnCoercion exCo

-- |Checks whether the function that is called is already defined with the same argument types
checkFunctionCall :: [Symbol] -> String -> [Either TypeCheckError ExplicitExpression ] -> Either TypeCheckError ExplicitExpression
checkFunctionCall [] fun args = Left $ UndefinedFunction $ "Undefined function: " ++ fun ++ " [" ++ show args ++ "]"
    ++ show [(typeName $ typeFromExpression x, toCardinality $ cardinalityCoercion $ returnCoercion x) | x <- rights args] ++ "]"
checkFunctionCall ((Func n a r):symbolMap) name args
    | not $ null $ lefts args = error $ show symbolMap ++ "\n" ++ show (lefts args)--Left $ ErrorInsideFunction (name ++ ": " ++ show (lefts args))
    | name == n = if all isRight coerce then Right $ ExplicitFunction name (zip (rights args) (rights coerce)) (MakeCoercion [MakeIdCoercion (fst r)] (MakeCardinalityIdCoercion (snd r)))
        else checkFunctionCall symbolMap name args
    | otherwise = checkFunctionCall symbolMap name args
    where
        argCoerce = map returnCoercion (rights args)
        coerce = zipWith coercionIncluded argCoerce (map createCoercion a)
checkFunctionCall (_:symbolMap) name args = checkFunctionCall symbolMap name args

getType :: String -> [Type] -> Either TypeCheckError Type
getType t [] = Left $ UndefinedType t
getType typ (t : ts) 
    | typ == typeName t = Right t
    | otherwise = getType typ ts 

typeIncluded :: (Type, Cardinality) -> (Type, Cardinality) -> Either TypeCheckError Coercion
typeIncluded (t1, c1) (t2, c2) =
    case t1 `isSubType` t2 of
        Left err -> Left err
        Right typeCoercion -> 
            case c1 `cardinalityIncluded` c2 of
                Left err -> Left err
                Right cardCoercion -> Right $ MakeCoercion typeCoercion cardCoercion

-- | Checks whether the first coercion can be transformed into the second coercion
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

-- |Look for a type attribute in a given type and its super types
findAttributeTypeRec :: [Type] -> String -> Type -> Either TypeCheckError ExplicitExpression
findAttributeTypeRec _ var (BasicType _) = Left $ UndefinedVariable var
findAttributeTypeRec defT var t = case findAttributeType var (getTypeAttributes defT t) of
    Left err -> case findAttributeTypeRec defT var (superType t) of
        Left err -> Left err
        Right (ExplicitVariable n (MakeCoercion tc cc)) -> Right $ 
            ExplicitVariable n 
                (MakeCoercion (MakeSuperCoercion t (toType $ head tc) : tc) cc)
        Right typ -> Right typ
    Right typ -> Right typ

-- |Find whether there is a attribute with the given name in the given type, and returns the attribute's type
findAttributeType :: String -> [TypeAttribute] -> Either TypeCheckError ExplicitExpression
findAttributeType var [] = Left $ UndefinedVariable var
findAttributeType var (t : ts)
    | var == attributeName t = Right $ ExplicitVariable var (MakeCoercion [MakeIdCoercion $ attributeType t] (MakeCardinalityIdCoercion $ Model.Type.cardinality t))
    | otherwise = findAttributeType var ts

-- |Removes the path expression from item in a list operation
-- ex: person map [item -> firstname] => person map [person -> firsname] 
replaceVar :: Expression -> String -> Expression
replaceVar (Variable "item") var = Variable var
replaceVar (PathExpression a b) var = PathExpression (replaceVar a var) b
replaceVar (Parens e) var = Parens (replaceVar e var)
replaceVar (ListUnaryOp o e) var = ListUnaryOp o (replaceVar e var)
replaceVar (ListOp o e c) var = ListOp o (replaceVar e var) c
replaceVar (Function f e) var = Function f [replaceVar ex var | ex <- e]
replaceVar (PrefixExp o e) var = PrefixExp o (replaceVar e var)
replaceVar (PostfixExp o e) var = PostfixExp o (replaceVar e var)
replaceVar (InfixExp o e1 e2) var = InfixExp o (replaceVar e1 var) (replaceVar e2 var)
replaceVar (IfSimple c e1) var = IfSimple (replaceVar c var) (replaceVar e1 var)
replaceVar (IfElse c e1 e2) var = IfElse (replaceVar c var) (replaceVar e1 var) (replaceVar e2 var)
replaceVar e _ = e

-- |Checks whether the first argument is a subtype of the second argument
isSubType :: Type -> Type -> Either TypeCheckError [TypeCoercion]
isSubType (BasicType "Empty") _ = Right [MakeIdCoercion (BasicType "Empty")]
isSubType (BasicType "Integer") (BasicType "Double") = Right [MakeTypeCoercion (BasicType "Integer") (BasicType "Double") "fromInteger"]
isSubType t (BasicType "Any") = Right [MakeIdCoercion t]
isSubType (BasicType x) y
    | x == typeName y = Right [MakeIdCoercion y]
    | otherwise = Left $ TypeMismatch x (typeName y)
isSubType x y
    | typeName x == typeName y = Right [MakeIdCoercion x]
    | otherwise = case isSubType (superType x) y of
        Left e -> Left e
        Right transforms -> Right $ MakeSuperCoercion x y : transforms

getNextItem :: [Symbol] -> String
getNextItem symbs
    | Var "item" (BasicType "Any") (OneBound 0) `notElem` symbs = "item"
    | otherwise = head ["list" ++ show x | x <- [1..], Var ("list" ++ show x) (BasicType "Any") (OneBound 0) `notElem` symbs]

-- |Finds the type attributes from a type in the symbol table
getTypeAttributes :: [Type] -> Type -> [TypeAttribute]
getTypeAttributes [] t = []
getTypeAttributes defT t
    | t `elem` defT = let typ = head [x | x <- defT, x == t] in
        [MakeTypeAttribute {attributeName = attributeName attr,
            attributeType = case getType (typeName $ attributeType attr) defT of
                Left _ -> toHaskell (attributeType attr)
                Right atTyp -> atTyp,
            Model.Type.cardinality = Model.Type.cardinality attr,
            attributeDescription = attributeDescription attr}
            | attr <- typeAttributes typ]
    | otherwise = []

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
cardinalityIncluded (Bounds (1, 1)) (Bounds (0, 1)) = Right $ MakeObject2MaybeCoercion (Bounds (1, 1)) (Bounds (0, 1))
-- |Transform object into list
cardinalityIncluded (Bounds (1, 1)) (OneBound 1) = Right $ MakeObject2ListCoercion (Bounds (1, 1)) (OneBound 1)
cardinalityIncluded (Bounds (1, 1)) (OneBound 0) = Right $ MakeObject2ListCoercion (Bounds (1, 1)) (OneBound 1)
-- |General
cardinalityIncluded (OneBound x) (OneBound y)
    | x == y = Right $ MakeCardinalityIdCoercion (OneBound x)
    | x > y = Right $ MakeListCardinalityCoercion (OneBound x) (OneBound y)
    | otherwise = Left $ CardinalityMismatch (OneBound x) (OneBound y)
cardinalityIncluded (Bounds (x1, y1)) (OneBound y)
    | x1 >= y = Right $ MakeListCardinalityCoercion (Bounds (x1, y1)) (OneBound y)
    | otherwise = Left $ CardinalityMismatch (Bounds (x1, y1)) (OneBound y)
cardinalityIncluded (OneBound x) (Bounds (x2, y2)) = Left $ CardinalityMismatch (OneBound x) (Bounds (x2, y2))
cardinalityIncluded (Bounds (x1, x2)) (Bounds (y1, y2))
    | x1 == y1 && x2 == y2 = Right $ MakeCardinalityIdCoercion (Bounds (x1, x2))
    | x1 >= y1 && x2 <= y2 = Right $ MakeListCardinalityCoercion (Bounds (x1, x2)) (Bounds (y1, y2))
    | otherwise = Left $ CardinalityMismatch (Bounds (x1, x2)) (Bounds (y1, y2))