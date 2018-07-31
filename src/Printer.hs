module Printer (
    prettyPrint
) where

import           Consts
import           Data.List (groupBy, intercalate)
import           Parser  (parseText, isTimeValue)
import           Helper    (hasDayUnit, hasHourUnit, hasMinuteUnit, hasTimeUnit)

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
