module FileParser (
    parseCodeLines
) where

import           Data.List
import           DataTypes (CodeLine (..), makeCodeLine)
import           Helper    (dropSpaces)

isEmptyLine :: String -> Bool
isEmptyLine str = length a == 0
    where a = dropSpaces str

isLineComment :: String -> Bool
isLineComment str = a b == '#'
    where a [] = 's'
          a xs = head xs
          b = dropSpaces str

lineFilter l = not a && not b
    where o = origin l
          a = isEmptyLine o
          b = isLineComment o

lineFold acc line = acc ++ a:[]
    where a = makeCodeLine (length acc) line

parseCodeLines :: String -> [CodeLine]
parseCodeLines str = filter lineFilter allLines
    where allLines = foldl lineFold [] $ lines str
