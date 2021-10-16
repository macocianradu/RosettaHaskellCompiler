module Model.Enum where

data EnumType = MakeEnum {
    enumName :: String,
    enumDescription :: Maybe String,
    enumValues :: [EnumValue]
} deriving (Show)

data EnumValue = MakeEnumValue {
    enumValueName :: String,
    enumValueDescription :: Maybe String,
    enumValueDisplayName :: Maybe String
} deriving (Show)