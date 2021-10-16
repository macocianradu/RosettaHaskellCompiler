module Model.Type where

import Data.Time.LocalTime()
import Model.Enum

data BasicType = String 
    | Integer 
    | Double 
    | Boolean 
    | TimeOfDay
    deriving (Show)

data Type = 
    TypeFromBasicType BasicType
    | TypeFromEnum EnumType
    | MakeType {
        typeName :: String,
        typeDescription :: Maybe String,
        typeAttributes :: [TypeAttribute]
    }
    deriving (Show)

data TypeAttribute = MakeTypeAttribute {
    attributeName :: String,
    attributeType :: String,
    cardinality :: Cardinality,
    attributeDescription :: Maybe String
} deriving (Show)

data Cardinality = 
    ZeroOrOne
    | ExactlyOne
    | OneOrMore             -- One or more
    | ZeroOrMore            -- Zero or more
    deriving (Show)