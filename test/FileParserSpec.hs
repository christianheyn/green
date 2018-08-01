module FileParserSpec (spec) where

import System.IO
import           FileParser    (parseCodeLines)
import           Test.Hspec

-- _TEST_FILE_1 = "./test-files/test1.green"

_TEST_STRING_1 = "\nLine1\n    #comment\nLine3\nLine4\n\n"

spec :: Spec
spec = do
    -- handle1 <- openFile _TEST_FILE_1 ReadMode
    -- fileContent <- hGetContents handle1
    describe "parseCodeLines" $ do
        it "###" $ do
            (length $ parseCodeLines _TEST_STRING_1) `shouldBe` 3
