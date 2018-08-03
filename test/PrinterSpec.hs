module PrinterSpec (spec) where

import           Printer    (prettyPrint)
import           Test.Hspec

spec :: Spec
spec = do

    describe "prettyPrint" $ do
        it "wraps timeValues with color strings" $ do
            prettyPrint "3d und 2h and .5m" `shouldBe`  "\ESC[3m\ESC[1m3d\ESC[0m\ESC[2m \ESC[0m\ESC[2mund\ESC[0m\ESC[2m \ESC[0m\ESC[3m\ESC[1m2h\ESC[0m\ESC[2m \ESC[0m\ESC[2mand\ESC[0m\ESC[2m \ESC[0m\ESC[3m\ESC[1m.5m\ESC[0m"
