module Estimate (
    isTimeValue
    , getAllTimeValues
    , resolvePointNumber
    , parseStrToMinutes
    , parseText
    , highlihgtTimeValues
) where

import Data.List (groupBy, intercalate)

_TIME_UNITS = ["d", "h", "m"]

data TimeValue = TimeValue {
    value :: Float
    , unit :: String
    , origin :: String
}

isSpaceChar :: Char -> Bool
isSpaceChar x = x `elem` [' ', '\t', '\n']

bothSpaces :: Char -> Char -> Bool
bothSpaces x y = (isSpaceChar x) == (isSpaceChar y)

isNumericChar :: Char -> Bool
isNumericChar x = x `elem` '-':'.':['0'..'9']

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

mapFloat :: String -> TimeValue
mapFloat x = TimeValue {
        value = resolvePointNumber $ init x
        , unit = (last x):[]
        , origin = x
    }

resolveTime :: Float -> TimeValue -> Float
resolveTime hoursPerDay tV
    | unit tV == "d" = (value tV) * hoursPerDay * 60
    | unit tV == "h" = (value tV) * 60
    | otherwise = (value tV)

-- export
parseStrToMinutes :: Float -> String -> Float
parseStrToMinutes hoursPerDay str = foldl (+) 0 c
    where c = map (resolveTime hoursPerDay) b
          b = map mapFloat a
          a = getAllTimeValues str

-- export
parseText :: String -> [String]
parseText str = groupBy bothSpaces str

wrapHighlight str = if isTimeValue str
    then "\x1b[45m " ++ str ++ " \x1b[0m"
    else str

-- export
highlihgtTimeValues :: String -> String
highlihgtTimeValues str = intercalate "" $
    map wrapHighlight $
    parseText str
