module Main where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Estimate (
        isTimeValue
        , getAllTimeValues
        , resolvePointNumber
        , parseStrToMinutes
    )

prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs

main :: IO ()
main = hspec $ do
    describe "Estimate" $ do

        describe "isTimeValue" $ do
            it "returns false on empty string" $ do
                isTimeValue "" `shouldBe` False
            it "false on string contains just of alphabetical chars" $ do
                isTimeValue "no" `shouldBe` False
            it "false on string contains just of numerical chars" $ do
                isTimeValue "34" `shouldBe` False
            it "false on string contains of numerical and alphabetical chars in wrong order" $ do
                isTimeValue "m34" `shouldBe` False
            it "false on string contains more than one point" $ do
                isTimeValue "0..3m" `shouldBe` False
            it "true on string contains of numerical and alphabetical(d, h, m) chars in right order" $ do
                isTimeValue "3d" `shouldBe` True
                isTimeValue "12h" `shouldBe` True
                isTimeValue "3m" `shouldBe` True
                isTimeValue "3.0m" `shouldBe` True
                isTimeValue "0.3m" `shouldBe` True
                isTimeValue "0.m" `shouldBe` True

        describe "getAllTimeValues" $ do
            it "returns empty list on empty string" $ do
                getAllTimeValues "" `shouldBe` []
            it "returns empty list when no timevalue found" $ do
                getAllTimeValues "test test2 333" `shouldBe` []
            it "returns list with timevalues" $ do
                getAllTimeValues "3m test2 5h" `shouldBe` ["3m", "5h"]

        describe "resolvePointNumber" $ do
            it "adds zero to start when point is head" $ do
                resolvePointNumber ".9" `shouldBe` 0.9
            it "adds zero to end when point is last" $ do
                resolvePointNumber "9." `shouldBe` 9.0
            it "returns float" $ do
                resolvePointNumber "9.4" `shouldBe` 9.4
                resolvePointNumber "9" `shouldBe` 9.0

        describe "parseStrToMinutes" $ do
            it "gives 0 when no str does not contains of timevalues" $ do
                parseStrToMinutes 8 "" `shouldBe` 0
                parseStrToMinutes 8 "a b c 6" `shouldBe` 0
            it "calculates time by timevalues" $ do
                parseStrToMinutes 8 "1h init" `shouldBe` 60.0
                parseStrToMinutes 8 "2h refactoring" `shouldBe` 120.0
                parseStrToMinutes 8 "1m sleep" `shouldBe` 1.0
                parseStrToMinutes 8 "0.5h coffee" `shouldBe` 30.0
                parseStrToMinutes 8 "0.5d coding" `shouldBe` 60.0 * 4
                parseStrToMinutes 8 ".5d coding" `shouldBe` 60.0 * 4
