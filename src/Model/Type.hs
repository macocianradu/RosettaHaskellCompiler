module Model.Type where

import Data.Time.LocalTime()

-- |The representation of a Rosetta data type
data Type = MakeType {
        typeName :: String,
        superType :: Maybe Type,
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