module Parser.EnumSpec where

import Test.Hspec
import Model.Enum
import qualified Data.Text as Text
import Text.Megaparsec
import Parser.Enum
import Test.Hspec.Megaparsec
  
spec :: Spec
spec = do
    describe "Testing enum parsing" $ do
        it "[Test 1]" $ do
            plainText <- readFile "resources/Enums/testEnum1.rosetta"
            parse enumParser "" (Text.pack plainText) `shouldParse` head enums
        it "[Test 2]" $ do
            plainText <- readFile "resources/Enums/testEnum2.rosetta"
            parse enumParser "" (Text.pack plainText) `shouldParse` (enums !! 1)
        it "[Test 3]" $ do
            plainText <- readFile "resources/Enums/testEnum3.rosetta"
            parse enumParser "" (Text.pack plainText) `shouldParse` (enums !! 2)
        it "[Test 4]" $ do
            plainText <- readFile "resources/Enums/testEnum4.rosetta"
            parse enumParser "" (Text.pack plainText) `shouldParse` (enums !! 3)
        it "[Test 5]" $ do
            plainText <- readFile "resources/Enums/testEnum5.rosetta"
            parse enumParser "" `shouldFailOn` Text.pack plainText 
        it "[Test 6]" $ do
            plainText <- readFile "resources/Enums/testEnum6.rosetta"
            parse enumParser "" `shouldFailOn` Text.pack plainText
                
    
enums :: [EnumType]
enums = [
    MakeEnum {enumName = "PeriodEnum", 
    enumDescription = Just "The enumerated values to specified the period, e.g. day, week.", 
    enumValues = [MakeEnumValue {enumValueName = "D", enumValueDescription = Just "Day", enumValueDisplayName = Just "day"},
        MakeEnumValue {enumValueName = "M", enumValueDescription = Just "Month", enumValueDisplayName = Just "month"},
        MakeEnumValue {enumValueName = "Y", enumValueDescription = Just "Year", enumValueDisplayName = Just "year"}]},
        
    MakeEnum {enumName = "EnumWithoutDisplay", 
    enumDescription = Just "The enumerated values to specified the period, e.g. day, week.", 
    enumValues = [MakeEnumValue {enumValueName = "D", enumValueDescription = Just "Day", enumValueDisplayName = Nothing},
        MakeEnumValue {enumValueName = "M", enumValueDescription = Just "Month", enumValueDisplayName = Nothing},
        MakeEnumValue {enumValueName = "Y", enumValueDescription = Just "Year", enumValueDisplayName = Nothing}]},
        
        
    MakeEnum {enumName = "EnumWithoutDescription", 
    enumDescription = Nothing, 
    enumValues = [MakeEnumValue {enumValueName = "X", enumValueDescription = Nothing, enumValueDisplayName = Just "xs"},
        MakeEnumValue {enumValueName = "Y", enumValueDescription = Nothing, enumValueDisplayName = Just "ys"}]},
        
    MakeEnum {enumName = "Wrong", 
    enumDescription = Nothing, 
    enumValues = [MakeEnumValue {enumValueName = "A", enumValueDescription = Just "asd", enumValueDisplayName = Nothing}]}]