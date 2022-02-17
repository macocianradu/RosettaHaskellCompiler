module Model.TypeSpec where

import Test.Hspec
import Model.Type

spec :: Spec
spec = do
    describe "Testing cardinality addition" $ do
        it "[Test add 1]" $ do
            head cards1 .+ head cards2 `shouldBe` head cardsSum
        it "[Test add 2]" $ do
            cards1 !! 1 .+ cards2 !! 1 `shouldBe` cardsSum !! 1
        it "[Test add 3]" $ do
            cards1 !! 2 .+ cards2 !! 2 `shouldBe` cardsSum !! 2
        it "[Test add 4]" $ do
            cards1 !! 3 .+ cards2 !! 3 `shouldBe` cardsSum !! 3
        it "[Test add 5]" $ do
            cards1 !! 4 .+ cards2 !! 4 `shouldBe` cardsSum !! 4
        it "[Test add 6]" $ do
            cards1 !! 5 .+ cards2 !! 5 `shouldBe` cardsSum !! 5
        it "[Test add 7]" $ do
            cards1 !! 6 .+ cards2 !! 6 `shouldBe` cardsSum !! 6
        it "[Test add 8]" $ do
            cards1 !! 7 .+ cards2 !! 7 `shouldBe` cardsSum !! 7
        it "[Test add 9]" $ do
            cards1 !! 8 .+ cards2 !! 8 `shouldBe` cardsSum !! 8
        it "[Test add 10]" $ do
            cards1 !! 9 .+ cards2 !! 9 `shouldBe` cardsSum !! 9
    describe "Testing smallest cardinality" $ do
        it "[Test smallest 1]" $ do
            smallestBound (head cards1) (head cards2) `shouldBe` head smallestCards
        it "[Test smallest 2]" $ do
            smallestBound (cards1 !! 1) (cards2 !! 1) `shouldBe` smallestCards !! 1
        it "[Test smallest 3]" $ do
            smallestBound (cards1 !! 2) (cards2 !! 2) `shouldBe` smallestCards !! 2
        it "[Test smallest 4]" $ do
            smallestBound (cards1 !! 3) (cards2 !! 3) `shouldBe` smallestCards !! 3
        it "[Test smallest 5]" $ do
            smallestBound (cards1 !! 4) (cards2 !! 4) `shouldBe` smallestCards !! 4
        it "[Test smallest 6]" $ do
            smallestBound (cards1 !! 5) (cards2 !! 5) `shouldBe` smallestCards !! 5
        it "[Test smallest 7]" $ do
            smallestBound (cards1 !! 6) (cards2 !! 6) `shouldBe` smallestCards !! 6
        it "[Test smallest 8]" $ do
            smallestBound (cards1 !! 7) (cards2 !! 7) `shouldBe` smallestCards !! 7
        it "[Test smallest 9]" $ do
            smallestBound (cards1 !! 8) (cards2 !! 8) `shouldBe` smallestCards !! 8
        it "[Test smallest 10]" $ do
            smallestBound (cards1 !! 9) (cards2 !! 9) `shouldBe` smallestCards !! 9
        

cards1 :: [Cardinality]
cards1 = 
    [Bounds (0, 20), Bounds (10, 15), Bounds (25, 50), Bounds (15, 16), NoBounds,      OneBound 25,    OneBound 2, OneBound 1, NoBounds,   NoBounds]

cards2 :: [Cardinality]
cards2 = 
    [Bounds (2, 4),  Bounds (4, 45),  OneBound 6,      NoBounds,        Bounds (2, 5), Bounds (2, 30), OneBound 5, NoBounds,   OneBound 5, NoBounds]

cardsSum :: [Cardinality]
cardsSum = 
    [Bounds (2, 24), Bounds (14, 60), OneBound 31,     OneBound 15,     OneBound 2,    OneBound 27,    OneBound 7, OneBound 1, OneBound 5, NoBounds]

smallestCards :: [Cardinality]
smallestCards =
    [Bounds (0, 20), Bounds (4, 45),  OneBound 6,      NoBounds,        NoBounds,      OneBound 2,     OneBound 2, NoBounds,   NoBounds,   NoBounds]