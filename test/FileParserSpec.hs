module FileParserSpec (spec) where

import           FileParser (parseCodeLines, CodeLine (..), getCodeLineType)
import           System.IO
import           Test.Hspec

-- _TEST_FILE_1 = "./test-files/test1.green"

_TEST_STRING_1 = "\nLine1\n    #comment\nLine3\n    Line4\n\n"
_TEST_STRING_2 = "Line1\n    #comment\nLine3\n    @feature{}\n\n"

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
        it "gives list on string contains of lines with comments" $ do
            (length $ parseCodeLines "#comment1\n\n#comment2\n   ") `shouldBe` 2
        it "gives list of CodeLines" $ do
            (length $ parseCodeLines _TEST_STRING_1) `shouldBe` 4
            let parsedLines = parseCodeLines _TEST_STRING_1
            (origin $ head parsedLines) `shouldBe` "Line1"
            (origin $ last parsedLines) `shouldBe` "Line4"
            (indention $ last parsedLines) `shouldBe` 4
        it "every CodeLine stores its index (number)" $ do
            let parsedLines = parseCodeLines _TEST_STRING_1
            (number $ head parsedLines) `shouldBe` 1
            (number $ last parsedLines) `shouldBe` 4

            let parsedLines2 = parseCodeLines _TEST_STRING_2
            (number $ head parsedLines2) `shouldBe` 0
            (number $ last parsedLines2) `shouldBe` 3
    describe "getCodeLineType" $ do
        it "gives pair with line type" $ do
            getCodeLineType "   @feature{}" `shouldBe` ("OBJECT", "feature")
            getCodeLineType "   # a comment" `shouldBe` ("COMMENT", "?")
            getCodeLineType " @tags[]" `shouldBe` ("LIST", "tags")
            getCodeLineType " @title" `shouldBe` ("PROPERTY", "title")
            getCodeLineType "    some text" `shouldBe` ("VALUE", "?")
            getCodeLineType " @" `shouldBe` ("SYNTAX_ERROR","MissingTagName")
            getCodeLineType " @ space" `shouldBe` ("SYNTAX_ERROR","MissingTagName")
