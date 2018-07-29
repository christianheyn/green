module Estimate (
      hasDayUnit
    , hasHourUnit
    , hasMinuteUnit
    , hasTimeUnit
    , isTimeValue
    , getAllTimeValues
    , resolvePointNumber
    , parseStrToMinutes
) where

import           Data.List (groupBy, isSuffixOf, isPrefixOf)

_DAY_UNITS = ["d", "day", "days"]
_HOUR_UNITS = ["h", "hour", "hours"]
_MINUTES_UNITS = ["m", "min", "minute", "minutes"]
_TIME_UNITS = _DAY_UNITS ++ _HOUR_UNITS ++_MINUTES_UNITS

makeUnitChecker unitsList str = True `elem` map findUnits unitsList
    where findUnits = (\x -> x `isSuffixOf` str)

hasDayUnit = makeUnitChecker _DAY_UNITS
hasHourUnit = makeUnitChecker _HOUR_UNITS
hasMinuteUnit = makeUnitChecker _MINUTES_UNITS
hasTimeUnit = makeUnitChecker _TIME_UNITS

data TimeValue = TimeValue {
      value  :: Float
    , unit   :: String
    , origin :: String
}

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
          hasUnit = hasTimeUnit $ last g

-- export
getAllTimeValues :: String -> [String]
getAllTimeValues str = filter isTimeValue $ words str

-- export
resolvePointNumber :: String -> Float
resolvePointNumber str
    | length str /= length (takeWhile isNumericChar str) = 0
    | str == [] || str == "." || str == "-" = 0
    | head str == '.' = read $ ('0':str)
    | last str == '.' = read $ str ++ ('0':[])
    | "-." `isPrefixOf` str = read $ "-0" ++ tail str
    | otherwise = read str

mapFloat :: String -> TimeValue
mapFloat x = TimeValue {
          value = resolvePointNumber $ head $ groupByNumeric x
        , unit = last $ groupByNumeric x
        , origin = x
    }

resolveTime :: Float -> TimeValue -> Float
resolveTime hoursPerDay tV
    | unit tV `elem` _DAY_UNITS = (value tV) * hoursPerDay * 60
    | unit tV `elem` _HOUR_UNITS = (value tV) * 60
    | otherwise = (value tV)

-- export
parseStrToMinutes :: Float -> String -> Float
parseStrToMinutes hoursPerDay str = foldl (+) 0 c
    where c = map (resolveTime hoursPerDay) b
          b = map mapFloat a
          a = getAllTimeValues str
