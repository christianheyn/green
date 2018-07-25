module Main where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Estimate (isTimeValue)

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
