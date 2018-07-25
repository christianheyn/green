module Main where

import Estimate (isTimeValue)

main :: IO ()
main = print $ isTimeValue "2d"
