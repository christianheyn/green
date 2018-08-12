module ParserSpec (spec) where

import           Parser                (getAllTimeValues, isTimeValue,
                                        parseStrToMinutes, parseText,
                                        resolvePointNumber)
import           Test.Hspec
import           Test.Hspec.QuickCheck (modifyMaxSize)
import           Test.QuickCheck

prop_resolvePointNumber str = a + (-1 * a) == 0
    where a = resolvePointNumber str

spec :: Spec
spec = do
    describe "isTimeValue" $ do
        it "returns false on empty string" $ do
            isTimeValue "" `shouldBe` False
        it "returns false on string contains just of alphabetical chars" $ do
            isTimeValue "no" `shouldBe` False
        it "returns false on string contains just of numerical chars" $ do
            isTimeValue "34" `shouldBe` False
        it "returns false on string contains of numerical and alphabetical chars in wrong order" $ do
            isTimeValue "m34" `shouldBe` False
        it "returns false on string contains more than one point" $ do
            isTimeValue "0..3m" `shouldBe` False
        it "returns true on string representing a valid TimeValue" $ do
            isTimeValue "3d" `shouldBe` True
            isTimeValue "12h" `shouldBe` True
            isTimeValue "3m" `shouldBe` True
            isTimeValue "3.0m" `shouldBe` True
            isTimeValue "0.3m" `shouldBe` True
            isTimeValue "0.m" `shouldBe` True
            isTimeValue "0.w" `shouldBe` True

    describe "getAllTimeValues" $ do
        it "returns empty list on empty string" $ do
            getAllTimeValues "" `shouldBe` []
        it "returns empty list when no timevalue found" $ do
            getAllTimeValues "test test2 333" `shouldBe` []
        it "returns list with timevalues" $ do
            getAllTimeValues "3m 3minute 3minutes" `shouldBe` ["3m", "3minute", "3minutes"]
            getAllTimeValues "3h 3hour 3hours" `shouldBe` ["3h", "3hour", "3hours"]
            getAllTimeValues "3d 3day 3days" `shouldBe` ["3d", "3day", "3days"]
            getAllTimeValues "3w 3week 3weeks" `shouldBe` ["3w", "3week", "3weeks"]

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
        modifyMaxSize (const 1000) $
            it "works with every string" $
                property prop_resolvePointNumber

    describe "parseStrToMinutes" $ do
        it "gives 0 when str does not contains of timevalues" $ do
            parseStrToMinutes 8 "" `shouldBe` 0
            parseStrToMinutes 8 "a b c 6" `shouldBe` 0
        it "calculates weeks from string" $ do -- Days
            parseStrToMinutes 8 "1w init" `shouldBe` 2400.0
            parseStrToMinutes 8 "1week init" `shouldBe` 2400.0
            parseStrToMinutes 8 "2weeks init" `shouldBe` 4800.0
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
            parseStrToMinutes 8 "1minute init" `shouldBe` 1.0
            parseStrToMinutes 8 "4minutes init" `shouldBe` 4.0
            parseStrToMinutes 8 "0.3minute init" `shouldBe` 0.3
            parseStrToMinutes 8 "0.3333minute init" `shouldBe` 0.3333
        it "calculates mixed units from string" $ do
            parseStrToMinutes 8 "1m 1h 1d" `shouldBe` 541.0
            parseStrToMinutes 8 "1.0m 1.0h 1.0d" `shouldBe` 541.0
            parseStrToMinutes 8 "2.0m 2.0h 2.0d" `shouldBe` 541.0 * 2
            parseStrToMinutes 8 "2.m 2.h 2.d" `shouldBe` 541.0 * 2
            parseStrToMinutes 8 "1minute 1hour 1day" `shouldBe` 541.0
            parseStrToMinutes 8 "1minute 1hour 1day" `shouldBe` 541.0
            parseStrToMinutes 8 "1.2minute 1.2222hour 1.98day -.932m" `shouldBe` 1024.0
        it "calculates negativ unit numbers" $ do
            parseStrToMinutes 8 "1m -1m" `shouldBe` 0
            parseStrToMinutes 8 "1m something -1m" `shouldBe` 0
            parseStrToMinutes 8 "1day something -4hours" `shouldBe` 240
            parseStrToMinutes 8 "1.5day something" `shouldBe` (1.5 * 8 * 60)
            -- TODO add more units
    describe "parseText" $ do
        it "returns empty list on empty string" $ do
            parseText "" `shouldBe` []
        it "returns words seperated from spaces as list" $ do
            parseText "test test2  test3" `shouldBe` ["test", " ", "test2", "  ", "test3"]
            parseText "  3h 2d" `shouldBe` ["  ", "3h", " ", "2d"]
