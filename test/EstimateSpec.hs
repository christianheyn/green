module EstimateSpec (spec) where

import           Estimate        (getAllTimeValues, isTimeValue,
                                  parseStrToMinutes, resolvePointNumber)
import           Test.Hspec
import           Test.QuickCheck

prop_resolvePointNumber str = a <= 0 || a > 0
    where a = resolvePointNumber str

check1000Times = quickCheckWith stdArgs { maxSuccess = 1000 }

spec :: Spec
spec = do
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
        it "returns negative numbers" $ do
            resolvePointNumber "-9.4" `shouldBe` -9.4
            resolvePointNumber "-9" `shouldBe` -9.0
            resolvePointNumber "-9" `shouldBe` -9.0
            resolvePointNumber "-.9" `shouldBe` -0.9
            check1000Times prop_resolvePointNumber
    describe "parseStrToMinutes" $ do
        it "gives 0 when str does not contains of timevalues" $ do
            parseStrToMinutes 8 "" `shouldBe` 0
            parseStrToMinutes 8 "a b c 6" `shouldBe` 0
        it "calculates days from string" $ do -- Days
            parseStrToMinutes 8 "1d init" `shouldBe` 480.0
            parseStrToMinutes 8 "1day init" `shouldBe` 480.0
            parseStrToMinutes 8 "2days init" `shouldBe` 960.0
            parseStrToMinutes 9 "2days init" `shouldBe` 1080.0
        it "calculates hours from string" $ do -- Hours
            parseStrToMinutes 8 "1h init" `shouldBe` 60.0
            parseStrToMinutes 8 "1hour init" `shouldBe` 60.0
            parseStrToMinutes 8 "1hours init" `shouldBe` 60.0
        it "calculates minutes from string" $ do -- Minutes
            parseStrToMinutes 8 "1m init" `shouldBe` 1.0
            parseStrToMinutes 8 "1min init" `shouldBe` 1.0
            parseStrToMinutes 8 "1minute init" `shouldBe` 1.0
            parseStrToMinutes 8 "4minutes init" `shouldBe` 4.0
        it "calculates mixed units from string" $ do
            parseStrToMinutes 8 "1m 1h 1d" `shouldBe` 541.0
            parseStrToMinutes 8 "1.0m 1.0h 1.0d" `shouldBe` 541.0
            parseStrToMinutes 8 "2.0m 2.0h 2.0d" `shouldBe` 541.0 * 2
            parseStrToMinutes 8 "2.m 2.h 2.d" `shouldBe` 541.0 * 2
            parseStrToMinutes 8 "1min 1hour 1day" `shouldBe` 541.0
            parseStrToMinutes 8 "1minute 1hour 1day" `shouldBe` 541.0
        it "calculates negativ unit numbers" $ do
            parseStrToMinutes 8 "1m -1m" `shouldBe` 0
            parseStrToMinutes 8 "1m something -1m" `shouldBe` 0
            parseStrToMinutes 8 "1day something -4hours" `shouldBe` 240
            -- TODO add more units
