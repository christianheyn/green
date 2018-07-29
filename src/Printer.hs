module Printer (
      parseText
    , prettyPrint
) where

import           Data.List (groupBy, intercalate)
import           Estimate  (hasDayUnit, hasHourUnit, hasMinuteUnit, hasTimeUnit,
                            isTimeValue)

isSpaceChar :: Char -> Bool
isSpaceChar x = x `elem` [' ', '\t', '\n']

bothSpaces :: Char -> Char -> Bool
bothSpaces x y = (isSpaceChar x) == (isSpaceChar y)

-- export
parseText :: String -> [String]
parseText str = groupBy bothSpaces str

getColorStartStr str
    | hasDayUnit str = "\x1b[46m"
    | hasHourUnit str = "\x1b[44m"
    | otherwise = "\x1b[45m"

getColorEndStr = "\x1b[0m"

wrapHighlight str = if isTimeValue str
    then color ++ " " ++ str ++ " " ++ getColorEndStr
    else str
    where color = getColorStartStr str

-- export
prettyPrint :: String -> String
prettyPrint str =
    map (\x -> if x == ';' then '\n' else x) $
    intercalate "" $
    map wrapHighlight $
    parseText str
