module Model.Type where

import Data.Time.LocalTime()

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

data TypeAttribute = MakeTypeAttribute {
    attributeName :: String,
    attributeType :: Type,
    cardinality :: Cardinality,
    attributeDescription :: Maybe String
} deriving (Show)

--TODO use bounded class
data Cardinality = Bounds (Integer, Integer)
  | OneBound Integer
  | NoBounds 
  deriving Show