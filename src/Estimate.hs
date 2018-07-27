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

data TimeValue = TimeValue {
    value :: Float
    , unit :: String
    , origin :: String
}

mapFloat :: String -> TimeValue
mapFloat x = TimeValue {
        value = resolvePointNumber $ init x
        , unit = (last x):[]
        , origin = x
    }

resolveTime :: Float -> TimeValue -> Float
resolveTime hoursPerDay tM
    | unit tM == "d" = (value tM) * hoursPerDay * 60
    | unit tM == "h" = (value tM) * 60
    | otherwise = (value tM)

parseStrToMinutes :: Float -> String -> Float
parseStrToMinutes hoursPerDay str = foldl (+) 0 c
    where c = map (resolveTime hoursPerDay) b
          b = map mapFloat a
          a = getAllTimeValues str
