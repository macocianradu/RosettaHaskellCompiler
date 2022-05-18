module Model.Enum where
import Model.Type

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

convertEnumToType :: EnumType -> Type
convertEnumToType (MakeEnum name desc val) = MakeType name (BasicType "Object") desc (map (convertValueToAttribute typ) val) [MakeCondition Nothing (Keyword "one-of")] 
    where typ = MakeType name (BasicType "Object") desc [] [MakeCondition Nothing (Keyword "one-of")] 

convertValueToAttribute :: Type -> EnumValue -> TypeAttribute
convertValueToAttribute typ (MakeEnumValue name desc _) = MakeTypeAttribute name typ (Bounds (0, 1)) Nothing