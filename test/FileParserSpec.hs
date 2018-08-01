module FileParserSpec (spec) where

import           DataTypes  (CodeLine (..))
import           FileParser (parseCodeLines)
import           System.IO
import           Test.Hspec

-- _TEST_FILE_1 = "./test-files/test1.green"

_TEST_STRING_1 = "\nLine1\n    #comment\nLine3\n    Line4\n\n"

spec :: Spec
spec = do
    -- handle1 <- openFile _TEST_FILE_1 ReadMode
    -- fileContent <- hGetContents handle1
    describe "parseCodeLines" $ do
        it "gives empty list on empty string" $ do
            (length $ parseCodeLines "") `shouldBe` 0
        it "gives empty list on string contains of newlines" $ do
            (length $ parseCodeLines "\n\n\n") `shouldBe` 0
        it "gives empty list on string contains of spaces" $ do
            (length $ parseCodeLines "    ") `shouldBe` 0
        it "gives empty list on string contains of tabs" $ do
            (length $ parseCodeLines "      ") `shouldBe` 0
        it "gives empty list on string contains of lines with comments" $ do
            (length $ parseCodeLines "#comment1\n\n#comment2\n   ") `shouldBe` 0
        it "gives list of CodeLines" $ do
            (length $ parseCodeLines _TEST_STRING_1) `shouldBe` 3
            let parsedLines = parseCodeLines _TEST_STRING_1
            (origin $ head parsedLines) `shouldBe` "Line1"
            (origin $ last parsedLines) `shouldBe` "Line4"
            (indention $ last parsedLines) `shouldBe` 4
