module PrinterSpec (spec) where

import           Printer    (prettyPrint)
import           Test.Hspec

spec :: Spec
spec = do

    describe "prettyPrint" $ do
        it "wraps timeValues with color strings" $ do
            prettyPrint "3d und 2h and .5m" `shouldBe` "\ESC[46m 3d \ESC[0m und \ESC[44m 2h \ESC[0m and \ESC[45m .5m \ESC[0m"
