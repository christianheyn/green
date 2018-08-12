module Helper (
      bothNumeric
    , bothSeparator
    , hasWeekUnit
    , hasDayUnit
    , hasHourUnit
    , hasMinuteUnit
    , hasTimeUnit
    , isNumericChar
    , dropSpaces
    , isEmptyLine
    , isLineComment
) where

import           Consts
import           Data.List (isSuffixOf, isPrefixOf)

isNumericChar :: Char -> Bool
isNumericChar x = x `elem` '-':'.':['0'..'9']

bothNumeric :: Char -> Char -> Bool
bothNumeric x y = (isNumericChar x) == (isNumericChar y)

isSeparatorChar :: Char -> Bool
isSeparatorChar x = x `elem` a
    where a = [' ', '\t', '\n'] ++ [',', ';', '|', '+']

bothSeparator :: Char -> Char -> Bool
bothSeparator x y = (isSeparatorChar x) == (isSeparatorChar y)

makeUnitChecker unitsList str = True `elem` map findUnits unitsList
    where findUnits = (\x -> x `isSuffixOf` str)

hasWeekUnit = makeUnitChecker _WEEK_UNITS
hasDayUnit = makeUnitChecker _DAY_UNITS
hasHourUnit = makeUnitChecker _HOUR_UNITS
hasMinuteUnit = makeUnitChecker _MINUTES_UNITS
hasTimeUnit = makeUnitChecker _TIME_UNITS

dropSpaces :: String -> String
dropSpaces = dropWhile (`elem` [' ', '\t'])

isEmptyLine :: String -> Bool
isEmptyLine str = length a == 0
    where a = dropSpaces str

isLineComment :: String -> Bool
isLineComment str = a b == '#'
    where a [] = 's'
          a xs = head xs
          b = dropSpaces str
