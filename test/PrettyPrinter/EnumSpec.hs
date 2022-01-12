module PrettyPrinter.EnumSpec where

import Test.Hspec
import Model.Enum
import PrettyPrinter.Enum
  
spec :: Spec
spec = do
    describe "Testing enum parsing" $ do
        it "[Test 1]" $ do
            plainText <- readFile "resources/Enums/haskellEnum1.hs"
            printEnum (head enums) `shouldBe` plainText
        it "[Test 2]" $ do
            plainText <- readFile "resources/Enums/haskellEnum2.hs"
            printEnum (enums !! 1) `shouldBe` plainText
        it "[Test 3]" $ do
            plainText <- readFile "resources/Enums/haskellEnum3.hs"
            printEnum (enums !! 2) `shouldBe` plainText
                
    
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
        MakeEnumValue {enumValueName = "Y", enumValueDescription = Nothing, enumValueDisplayName = Just "ys"}]}]