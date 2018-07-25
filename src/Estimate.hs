module Estimate (
    isTimeValue
    , getAllTimeValues
    , resolvePointNumber
    , parseStrToMinutes
) where

import Data.List (groupBy)

_TIME_UNITS = ["d", "h", "m"]

isNumericChar :: Char -> Bool
isNumericChar x = x `elem` '.':['0'..'9']

bothNumeric :: Char -> Char -> Bool
bothNumeric x y = (isNumericChar x) == (isNumericChar y)

groupByNumeric = groupBy bothNumeric

hasOnePoint :: String -> Bool
hasOnePoint x = (length $ filter ((==) '.') x) <= 1

-- export
isTimeValue :: String -> Bool
isTimeValue str = length g == 2 && hasUnit && (hasOnePoint $ head g)
    where g = groupByNumeric str
          hasUnit = (last g) `elem` _TIME_UNITS

-- export
getAllTimeValues :: String -> [String]
getAllTimeValues str = filter isTimeValue $ words str

-- export
resolvePointNumber :: String -> Float
resolvePointNumber str
    | str == [] = 0
    | head str == '.' = read $ ('0':str)
    | last str == '.' = read $ str ++ ('0':[])
    | otherwise = read str

mapFloat :: String -> (Float, String)
mapFloat x = (resolvePointNumber $ init x, (last x):[])

resolveTime :: Float -> (Float, String) -> Float
resolveTime hoursPerDay (n, unit)
    | unit == "d" = n * hoursPerDay * 60
    | unit == "h" = n * 60
    | otherwise = n

-- TODO: hoursPerDay
parseStrToMinutes :: Float -> String -> Float
parseStrToMinutes hoursPerDay str = foldl (+) 0 c
    where c = map (resolveTime hoursPerDay) b
          b = map mapFloat a
          a = getAllTimeValues str


