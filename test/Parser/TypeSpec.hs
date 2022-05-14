module Parser.TypeSpec where

import Test.Hspec
import Text.Megaparsec
import qualified Data.Text as Text
import Model.Type
import Test.Hspec.Megaparsec
import Parser.Type
  
spec :: Spec
spec = do
    describe "Testing type parsing" $ do
        it "[Test 1]" $ do
            plainText <- readFile "resources/Types/testType1.rosetta"
            parse typeParser "" (Text.pack plainText) `shouldParse` head types
        it "[Test 2]" $ do
            plainText <- readFile "resources/Types/testType2.rosetta"
            parse typeParser "" (Text.pack plainText) `shouldParse` (types !! 1)
        it "[Test 3]" $ do
            plainText <- readFile "resources/Types/testType3.rosetta"
            parse typeParser "" (Text.pack plainText) `shouldParse` (types !! 2)
        it "[Test 4]" $ do
            plainText <- readFile "resources/Types/testType4.rosetta"
            parse typeParser "" (Text.pack plainText) `shouldParse` (types !! 3)
        it "[Test 5]" $ do
            plainText <- readFile "resources/Types/testType5.rosetta"
            parse typeParser "" (Text.pack plainText) `shouldParse` (types !! 4)
        it "[Test 6]" $ do
            plainText <- readFile "resources/Types/testType6.rosetta"
            parse typeParser "" (Text.pack plainText) `shouldParse` (types !! 5)
        it "[Test 7]" $ do
            plainText <- readFile "resources/Types/testType7.rosetta"
            parse typeParser "" (Text.pack plainText) `shouldParse` (types !! 6)
                
types :: [Type]
types = [
    MakeType {typeName = "Period",
        typeDescription = Just "description",
        superType = MakeType "Something" (BasicType "Object") Nothing [] [],
        typeAttributes = [MakeTypeAttribute {attributeName = "periodMultiplier", attributeType = MakeType "int" (BasicType "Object") Nothing [] [], cardinality = Bounds(1, 1), 
                attributeDescription = Just "A time period multiplier, e.g. 1, 2 or 3 etc. A negative value can be used when specifying an offset relative to another date, e.g. -2 days."},
            MakeTypeAttribute {attributeName = "testMany", attributeType = MakeType "TestType" (BasicType "Object") Nothing [] [], cardinality = OneBound 0, 
                attributeDescription = Just "Test many"},
            MakeTypeAttribute {attributeName = "testSome", attributeType = MakeType "TestSomeType" (BasicType "Object") Nothing [] [], cardinality = OneBound 1, 
                attributeDescription = Just "Test some"},
            MakeTypeAttribute {attributeName = "testMaybeOne", attributeType = MakeType "TestZeroOneType" (BasicType "Object") Nothing [] [], cardinality = Bounds (0, 1), 
                attributeDescription = Just "Test zero or one"},
            MakeTypeAttribute {attributeName = "testAll", attributeType = MakeType "Test" (BasicType "Object") Nothing [] [], cardinality = Bounds (2, 15), 
                attributeDescription = Just "Test all"}],
        conditions = []},
            
    MakeType {typeName = "TestType",
        typeDescription = Nothing,
        superType = BasicType "Object",
        typeAttributes = [MakeTypeAttribute {attributeName = "periodMultiplier", attributeType = MakeType "int" (BasicType "Object") Nothing [] [], cardinality = Bounds(1, 1), 
            attributeDescription = Nothing}],
        conditions = []},
        
        
    MakeType {typeName = "TestSomeType",
        typeDescription = Just "description",
        superType = BasicType "Object",
        typeAttributes = [MakeTypeAttribute {attributeName = "periodMultiplier", attributeType = MakeType "int" (BasicType "Object") Nothing [] [], cardinality = Bounds(1, 1), 
            attributeDescription = Just "A time period multiplier, e.g. 1, 2 or 3 etc. A negative value can be used when specifying an offset relative to another date, e.g. -2 days."}],
        conditions = []},
        
        
    MakeType {typeName = "TestZeroOneType",
        typeDescription = Nothing,
        superType = MakeType "Period" (BasicType "Object") Nothing [] [],
        typeAttributes = [MakeTypeAttribute {attributeName = "periodMultiplier", attributeType = MakeType "int" (BasicType "Object") Nothing [] [], cardinality = Bounds(1, 1), 
            attributeDescription = Nothing}],
        conditions = []},
            
    MakeType {typeName = "WrongCardinality", superType = BasicType "Object", typeDescription = Just "description", typeAttributes = [], conditions = []},
    
    MakeType {typeName = "WrongCardinality2", superType = BasicType "Object", typeDescription = Just "description", typeAttributes = [], conditions = []},
    
    MakeType {typeName = "MissingType", superType = BasicType "Object", typeDescription = Just "description", typeAttributes = [], conditions = []}

    ]