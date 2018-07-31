module Helper (
      bothNumeric
    , bothSpaces
    , hasDayUnit
    , hasHourUnit
    , hasMinuteUnit
    , hasTimeUnit
    , isNumericChar
) where

import           Consts
import           Data.List (groupBy, isSuffixOf, isPrefixOf)

isNumericChar :: Char -> Bool
isNumericChar x = x `elem` '-':'.':['0'..'9']

bothNumeric :: Char -> Char -> Bool
bothNumeric x y = (isNumericChar x) == (isNumericChar y)

isSpaceChar :: Char -> Bool
isSpaceChar x = x `elem` [' ', '\t', '\n']

bothSpaces :: Char -> Char -> Bool
bothSpaces x y = (isSpaceChar x) == (isSpaceChar y)


makeUnitChecker unitsList str = True `elem` map findUnits unitsList
    where findUnits = (\x -> x `isSuffixOf` str)

hasDayUnit = makeUnitChecker _DAY_UNITS
hasHourUnit = makeUnitChecker _HOUR_UNITS
hasMinuteUnit = makeUnitChecker _MINUTES_UNITS
hasTimeUnit = makeUnitChecker _TIME_UNITS
