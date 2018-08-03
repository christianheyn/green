module Parser (
      isTimeValue
    , getAllTimeValues
    , resolvePointNumber
    , parseStrToMinutes
    , parseText
) where

import           Consts
import           Data.List (groupBy, isPrefixOf, isSuffixOf)
import           Helper (
                      bothNumeric
                    , bothSeparator
                    , hasWeekUnit
                    , hasDayUnit
                    , hasHourUnit
                    , hasMinuteUnit
                    , hasTimeUnit
                    , isNumericChar
                    , dropSpaces
                )

data TimeValue = TimeValue {
      value  :: Float
    , unit   :: String
    , origin :: String
}

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

-- TODO: daysPerWeek as parameter
resolveTime :: Float -> TimeValue -> Float
resolveTime hoursPerDay timeValue'                          --  v  --
    | unit timeValue' `elem` _WEEK_UNITS = (value timeValue') * 5 * hoursPerDay * 60
    | unit timeValue' `elem` _DAY_UNITS = (value timeValue') * hoursPerDay * 60
    | unit timeValue' `elem` _HOUR_UNITS = (value timeValue') * 60
    | otherwise = (value timeValue')

-- export
parseStrToMinutes :: Float -> String -> Float
parseStrToMinutes hoursPerDay str = foldl (+) 0 c
    where c = map (resolveTime hoursPerDay) b
          b = map mapFloat a
          a = getAllTimeValues str

-- export
parseText :: String -> [String]
parseText str = groupBy bothSeparator str
