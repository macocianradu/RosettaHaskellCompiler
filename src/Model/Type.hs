module Model.Type where

-- |The representation of a Rosetta data type
data Type = MakeType {
        typeName :: String,
        superType :: Type,
        typeDescription :: Maybe String,
        typeAttributes :: [TypeAttribute],
        conditions :: [Condition]
    }
    | BasicType {
        typeName :: String
    }
    deriving (Show)

instance Eq Type where
    (==) (MakeType name _ _ _ _) (MakeType name2 _ _ _ _)
        | name == name2 = True
        | otherwise = False
    (==) (BasicType name) (BasicType name2)
        | name == name2 = True
        | otherwise = False
    (==) _ _ = False

data Condition = MakeCondition {
    conditionDescription :: Maybe String,
    expressionExpression :: Expression
} deriving (Show)

instance Eq Condition where
    (==) (MakeCondition _ ex) (MakeCondition _ ex2) = ex == ex2

-- |The representation of an expression
data Expression = Variable String
    | PathExpression Expression Expression
    | Keyword String
    | Int String
    | Real String
    | Boolean String
    | Enum String String
    | Empty
    | Parens Expression
    | ListUnaryOp String Expression
    | ListOp String Expression Expression
    | Reduce String Expression String String Expression 
    | List [Expression]
    | Function String [Expression]
    | PrefixExp String Expression
    | PostfixExp String Expression
    | InfixExp String Expression Expression
    | IfSimple Expression Expression
    | IfElse Expression Expression Expression
    deriving (Eq, Show)

data ExplicitExpression = ExplicitEmpty
    | ExplicitVariable {name :: String, returnCoercion :: Coercion}
    | Value {name :: String, returnCoercion :: Coercion}
    | ExplicitList [ExplicitExpression]
    | ExplicitEnumCall {name :: String, val :: String, returnCoercion :: Coercion}
    | ExplicitKeyword String
    | ExplicitListUnaryOp {op :: String, list :: ExplicitExpression, returnCoercion :: Coercion}
    | ExplicitListOp {op :: String, list :: ExplicitExpression, arg :: ExplicitExpression, returnCoercion :: Coercion}
    | ExplicitParens {expression :: ExplicitExpression, returnCoercion :: Coercion}
    | ExplicitReduce {op :: String, list :: ExplicitExpression, var1 :: String, var2 :: String, arg :: ExplicitExpression, returnCoercion :: Coercion}
    | ExplicitPath {super :: ExplicitExpression, sub :: ExplicitExpression, returnCoercion :: Coercion}
    | ExplicitFunction {name :: String, args :: [(ExplicitExpression, Coercion)], returnCoercion :: Coercion}
    | ExplicitIfSimple {cond :: (ExplicitExpression, Coercion), block1 :: (ExplicitExpression, Coercion), returnCoercion :: Coercion}
    | ExplicitIfElse {cond :: (ExplicitExpression, Coercion), block1 :: (ExplicitExpression, Coercion), block2 :: (ExplicitExpression, Coercion), returnCoercion :: Coercion}
    deriving (Eq)

changeCoercion :: ExplicitExpression -> Coercion -> ExplicitExpression
changeCoercion ExplicitEmpty _ = ExplicitEmpty
changeCoercion (ExplicitVariable n _) c = ExplicitVariable n c 
changeCoercion (Value n _) c = Value n c
changeCoercion (ExplicitList e) _ = ExplicitList e
changeCoercion (ExplicitKeyword n) _ = ExplicitKeyword n
changeCoercion (ExplicitParens e _) c = ExplicitParens e c 
changeCoercion (ExplicitPath s n _) c = ExplicitPath s n c
changeCoercion (ExplicitListOp n o ar _) c = ExplicitListOp n o ar c
changeCoercion (ExplicitReduce n o v1 v2 ar _) c = ExplicitReduce n o v1 v2 ar c
changeCoercion (ExplicitListUnaryOp n o _) c = ExplicitListUnaryOp n o c
changeCoercion (ExplicitFunction n args _) c = ExplicitFunction n args c 
changeCoercion (ExplicitIfSimple cond block _) c = ExplicitIfSimple cond block c
changeCoercion (ExplicitIfElse cond block block2 _) c = ExplicitIfElse cond block block2 c    
changeCoercion (ExplicitEnumCall n val _) c = ExplicitEnumCall n val c    

instance Show ExplicitExpression where
    show (ExplicitVariable name coer) = show $ "Variable: " ++ name
    show (Value name coer) = show $ "Value: " ++ name
    show (ExplicitList lst) = concatMap show lst
    show (ExplicitKeyword name) = show $ "Keyword: " ++ name
    show (ExplicitParens name coer) = show $ "(" ++ show name ++ ")"
    show (ExplicitPath super sub coer) = show $ "(->" ++ show super ++ " " ++ show sub ++ ")"
    show (ExplicitFunction name args coer) = show $ name ++ "(" ++ concatMap show args ++ ")" 
    show (ExplicitIfSimple cond block coer) = show $ "if" ++ show cond ++ " then " ++ show block
    show (ExplicitIfElse cond block1 block2 coer) = show $ "if" ++ show cond ++ " then " ++ show block1 ++ " else " ++ show block2
    show ExplicitEmpty = show "Empty"
    show (ExplicitListOp lst op ar coer) = show $ show lst ++ " " ++ show op ++ " " ++ show ar
    show (ExplicitReduce lst op v1 v2 ar coer) = show $ show lst ++ " " ++ show op ++ " " ++ show ar
    show (ExplicitListUnaryOp lst op coer) = show $ show lst ++ " " ++ show op
    show (ExplicitEnumCall n val coer) = show $ "Enumcall: " ++ n ++ "->" ++ val

data TypeCoercion =
    MakeIdCoercion {toType :: Type}
    | MakeSuperCoercion {fromType :: Type, toType :: Type}
    | MakeTypeCoercion {fromType :: Type, toType :: Type, transformType :: String}
    deriving (Eq, Show)

data CardinalityCoercion =
    MakeCardinalityIdCoercion {toCardinality :: Cardinality}
    | MakeListCardinalityCoercion {fromCardinality :: Cardinality, toCardinality :: Cardinality}
    | MakeNothing2MaybeCoercion {fromCardinality :: Cardinality, toCardinality :: Cardinality}
    | MakeNothing2ListCoercion {fromCardinality :: Cardinality, toCardinality :: Cardinality}
    | MakeMaybe2ListCoercion {fromCardinality :: Cardinality, toCardinality :: Cardinality}
    | MakeObject2MaybeCoercion {fromCardinality :: Cardinality, toCardinality :: Cardinality}
    | MakeObject2ListCoercion {fromCardinality :: Cardinality, toCardinality :: Cardinality}
    | MakeOneOfCoercion {toCardinality :: Cardinality}
    | MakeMaybe2ObjectCoercion {toCardinality :: Cardinality}
    deriving (Eq, Show)

-- |Used to handle polymorphism in Rosetta
data Coercion = MakeCoercion {typeCoercion :: [TypeCoercion], cardinalityCoercion :: CardinalityCoercion} deriving(Eq, Show)

-- |The representation of an attribute of a data type
data TypeAttribute = MakeTypeAttribute {
    attributeName :: String,
    attributeType :: Type,
    cardinality :: Cardinality,
    attributeDescription :: Maybe String
} deriving (Show)

-- |The representation of cardinality
data Cardinality =
    -- |The cardinality between two bounds (ex. 2 - 5)
  Bounds (Integer, Integer)
    -- |The cardinality starting from one bound until infinity (ex. 5 - *)
  | OneBound Integer
  deriving Show

instance Eq Cardinality where
    (==) (Bounds (x1, x2)) (Bounds (y1, y2))
        | x1 == y1 && x2 == y2 = True
        | otherwise = False
    (==) (OneBound x) (OneBound y) = x == y
    (==) _ _ = False

-- |Function to create the smallest cardinality that includes two others
smallestBound  :: Cardinality -> Cardinality -> Cardinality
smallestBound (OneBound x) (OneBound y) = OneBound $ min x y
smallestBound (OneBound x) (Bounds (y, _)) = smallestBound (OneBound x) (OneBound y)
smallestBound (Bounds (x, _)) (OneBound y) = smallestBound (OneBound x) (OneBound y)
smallestBound (Bounds (x1, x2)) (Bounds (y1, y2)) = Bounds (min x1 y1, max x2 y2)

lowerBound :: Cardinality -> Integer
lowerBound (Bounds (x, _)) = x
lowerBound (OneBound x) = x

upperBound :: Cardinality -> Integer
upperBound (Bounds (_, x)) = x
upperBound (OneBound _) = toInteger (maxBound :: Int)

-- |A function used to add two cardinalities    
addBounds :: Cardinality -> Cardinality -> Cardinality
addBounds (Bounds (x1, x2)) (Bounds (y1, y2)) = Bounds (x1 + y1, x2 + y2)
addBounds (Bounds (x1, _)) (OneBound y1) = OneBound (x1 + y1)
addBounds (OneBound x1) (Bounds (y1, y2)) = addBounds (Bounds (y1, y2)) (OneBound x1)
addBounds (OneBound x1) (OneBound y1) = OneBound (x1 + y1)

-- |Custom operator for adding cardinalities
infixl 5 .+
(.+) :: Cardinality -> Cardinality -> Cardinality
(.+) = addBounds

typeAndCardinality :: TypeAttribute -> (Type, Cardinality)
typeAndCardinality (MakeTypeAttribute _ typ crd _) = (typ, crd)

toHaskell :: Type -> Type
toHaskell a
    | typeName a == "int" = BasicType "Integer"
    | typeName a == "boolean" = BasicType "Boolean"
    | typeName a == "number" = BasicType "Double"
    | typeName a == "string" = BasicType "String"
    | otherwise = a

coercionType :: [TypeCoercion] -> Type
coercionType [] = BasicType "Empty"
coercionType [x] = toType x 
coercionType (x:rst) = coercionType rst

-- |Gets the final cardinality from a list of coercions
coercionCardinality :: [CardinalityCoercion] -> Cardinality
coercionCardinality [] = OneBound 0
coercionCardinality [x] = toCardinality x
coercionCardinality (x:rst) = coercionCardinality rst

createCoercion :: (Type, Cardinality) -> Coercion
createCoercion (t, c) = MakeCoercion [MakeIdCoercion t] (MakeCardinalityIdCoercion c)

anyListCoercion :: Coercion
anyListCoercion = MakeCoercion [MakeIdCoercion (BasicType "Any")] (MakeCardinalityIdCoercion (OneBound 0))

typeFromExpression :: ExplicitExpression -> Type
typeFromExpression = coercionType . typeCoercion . returnCoercion