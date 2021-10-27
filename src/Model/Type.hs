module Model.Type where

import Data.Time.LocalTime()

data Type = MakeType {
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