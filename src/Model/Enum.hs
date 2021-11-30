module Model.Enum where

-- |The representation of a Rosetta enum data type
data EnumType = MakeEnum {
    enumName :: String,
    enumDescription :: Maybe String,
    enumValues :: [EnumValue]
} deriving (Show, Eq)

-- |The representation of a Rosetta enum value type
data EnumValue = MakeEnumValue {
    enumValueName :: String,
    enumValueDescription :: Maybe String,
    enumValueDisplayName :: Maybe String
} deriving (Show, Eq)