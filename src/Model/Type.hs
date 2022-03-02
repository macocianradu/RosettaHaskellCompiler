module Model.Type where

-- |The representation of a Rosetta data type
data Type = MakeType {
        typeName :: String,
        superType :: Type,
        typeDescription :: Maybe String,
        typeAttributes :: [TypeAttribute]
    }
    | BasicType {
        typeName :: String
    }
    deriving (Show)

instance Eq Type where
    (==) (MakeType name _ _ _) (MakeType name2 _ _ _) 
        | name == name2 = True
        | otherwise = False 
    (==) (BasicType name) (BasicType name2)
        | name == name2 = True
        | otherwise = False
    (==) _ _ = False


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
    -- |The cardinality of no bounds (ex. * - *)
  | NoBounds 
  deriving Show
  
instance Eq Cardinality where
    (==) (Bounds (x1, x2)) (Bounds (y1, y2))
        | x1 == y1 && x2 == y2 = True
        | otherwise = False
    (==) (OneBound x) (OneBound y) = x == y
    (==) NoBounds NoBounds = True
    (==) _ _ = False

-- |Function to create the smallest cardinality that includes two others
smallestBound  :: Cardinality -> Cardinality -> Cardinality
smallestBound NoBounds _ = NoBounds
smallestBound _ NoBounds = NoBounds
smallestBound (OneBound x) (OneBound y) = OneBound $ min x y
smallestBound (OneBound x) (Bounds (y, _)) = smallestBound (OneBound x) (OneBound y)
smallestBound (Bounds (x, _)) (OneBound y) = smallestBound (OneBound x) (OneBound y)
smallestBound (Bounds (x1, x2)) (Bounds (y1, y2)) = Bounds (min x1 y1, max x2 y2)

-- |A function used to add two cardinalities    
addBounds :: Cardinality -> Cardinality -> Cardinality
addBounds (Bounds (x1, x2)) (Bounds (y1, y2)) = Bounds (x1 + y1, x2 + y2)
addBounds (Bounds (x1, _)) (OneBound y1) = OneBound (x1 + y1)
addBounds (Bounds (x1, _)) NoBounds = OneBound x1
addBounds (OneBound x1) (Bounds (y1, y2)) = addBounds (Bounds (y1, y2)) (OneBound x1)
addBounds (OneBound x1) (OneBound y1) = OneBound (x1 + y1)
addBounds (OneBound x1) NoBounds = OneBound x1
addBounds NoBounds (Bounds (y1, y2)) = addBounds (Bounds (y1, y2)) NoBounds
addBounds NoBounds (OneBound y1) = addBounds (OneBound y1) NoBounds
addBounds NoBounds NoBounds = NoBounds 
    
-- |Custom operator for adding cardinalities
infixl 5 .+
(.+) :: Cardinality -> Cardinality -> Cardinality
(.+) = addBounds
  
typeAndCardinality :: TypeAttribute -> (Type, Cardinality)
typeAndCardinality (MakeTypeAttribute _ typ crd _) = (typ, crd)

    
-- |Checks whether the first argument is a subtype of the second argument
isSubType :: Type -> Type -> Bool
isSubType (BasicType "Integer") (BasicType "Double") = True
isSubType _ (BasicType "Any") = True
isSubType _ (BasicType "Object") = False
isSubType x y 
    | x == y = True
    | otherwise = isSubType (superType x) y
    
-- |Checks whether the first cardinality is included into the second one 
cardinalityIncluded :: Cardinality -> Cardinality -> Bool
cardinalityIncluded _ NoBounds = True
cardinalityIncluded NoBounds _ = False
cardinalityIncluded (OneBound x) (OneBound y)
    | x >= y = True
    | otherwise = False
cardinalityIncluded (Bounds (x1, _)) (OneBound y)
    | x1 >= y = True
    | otherwise = False
cardinalityIncluded (OneBound _) (Bounds (_, _)) = False
cardinalityIncluded (Bounds (x1, x2)) (Bounds (y1, y2))
    | x1 >= y1 && x2 <= y2 = True
    | otherwise = False