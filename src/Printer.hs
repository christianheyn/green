module Printer (
    prettyPrint
) where

import           Consts
import           Data.List (groupBy, intercalate)
import           Parser  (parseText, isTimeValue)
import           Helper    (hasWeekUnit, hasDayUnit, hasHourUnit, hasMinuteUnit, hasTimeUnit)

getColorStartStr str = "\x1b[2m" -- "\x1b[7m\x1b[3m\x1b[1m"
--    | hasWeekUnit str = "\x1b[7m\x1b[3m\x1b[1m"
--    | hasDayUnit str = "\x1b[46m"
--    | hasHourUnit str = "\x1b[44m"
--    | otherwise = "\x1b[4m"

getColorEndStr = "\x1b[0m"

wrapHighlight str = if isTimeValue str
    then color ++ str ++ getColorEndStr
    else "\x1b[2m" ++ str ++ getColorEndStr
    where color = "\x1b[3m\x1b[1m"

-- export
prettyPrint :: String -> String
prettyPrint str =
    map (\x -> if x == ';' then '\n' else x) $
    intercalate "" $
    map wrapHighlight $
    parseText str
