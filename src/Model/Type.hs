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
    conditionName :: String,
    conditionDescription :: Maybe String,
    expressionExpression :: Expression
} deriving (Show)

-- |The representation of an expression
data Expression = Variable String
    | Int String
    | Real String
    | Boolean String
    | Empty
    | Parens Expression
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
    | ExplicitParens ExplicitExpression
    | ExplicitFunction {name :: String, args :: [(ExplicitExpression, Coercion)], returnCoercion :: Coercion}
    | ExplicitIfSimple {cond :: (ExplicitExpression, Coercion), block1 :: (ExplicitExpression, Coercion), returnCoercion :: Coercion}
    | ExplicitIfElse {cond :: (ExplicitExpression, Coercion), block1 :: (ExplicitExpression, Coercion), block2 :: (ExplicitExpression, Coercion), returnCoercion :: Coercion}
    deriving (Show)

data TypeCoercion =
    MakeIdCoercion {toType :: Type}
    | MakeSuperCoercion {fromType :: Type, toType :: Type}
    | MakeTypeCoercion {fromType :: Type, toType :: Type, transformType :: String}
    deriving (Show)

data CardinalityCoercion =
    MakeCardinalityIdCoercion {toCardinality :: Cardinality}
    | MakeListCardinalityCoercion {fromCardinality :: Cardinality, toCardinality :: Cardinality}
    | MakeNothing2MaybeCoercion {fromCardinality :: Cardinality, toCardinality :: Cardinality}
    | MakeNothing2ListCoercion {fromCardinality :: Cardinality, toCardinality :: Cardinality}
    | MakeMaybe2ListCoercion {fromCardinality :: Cardinality, toCardinality :: Cardinality}
    | MakeObject2MaybeCoercion {fromCardinality :: Cardinality, toCardinality :: Cardinality}
    | MakeObject2ListCoercion {fromCardinality :: Cardinality, toCardinality :: Cardinality}
    deriving (Show)

-- |Used to handle polymorphism in Rosetta
data Coercion = MakeCoercion {typeCoercion :: [TypeCoercion], cardinalityCoercion :: CardinalityCoercion} deriving(Show)


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
    | typeName a == "real" = BasicType "Double"
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