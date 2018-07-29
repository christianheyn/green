module PrinterSpec (spec) where

import Test.Hspec
import Printer (
          parseText
        , prettyPrint
    )

spec :: Spec
spec = do
    describe "parseText" $ do
        it "returns empty list on empty string" $ do
            parseText "" `shouldBe` []
        it "returns words seperated from spaces as list" $ do
            parseText "test test2  test3" `shouldBe` ["test", " ", "test2", "  ", "test3"]
            parseText "  3h 2d" `shouldBe` ["  ", "3h", " ", "2d"]
    describe "prettyPrint" $ do
        it "wraps timeValues with color strings" $ do
            prettyPrint "3d und 2h and .5m" `shouldBe` "\ESC[46m 3d \ESC[0m und \ESC[44m 2h \ESC[0m and \ESC[45m .5m \ESC[0m"
