module Estimate (isTimeValue) where

import Data.List (groupBy)

_TIME_UNITS = ["d", "h", "m"]

isNumericChar :: Char -> Bool
isNumericChar x = x `elem` '.':['0'..'9']

bothNumeric :: Char -> Char -> Bool
bothNumeric x y = (isNumericChar x) == (isNumericChar y)

groupByNumeric = groupBy bothNumeric

hasOnePoint :: String -> Bool
hasOnePoint x = (length $ filter ((==) '.') x) <= 1

isTimeValue :: String -> Bool
isTimeValue str = length g == 2 && hasUnit && (hasOnePoint $ head g)
    where
    g = groupByNumeric str
    hasUnit = (last g) `elem` _TIME_UNITS
