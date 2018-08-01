module FileParser (
    parseCodeLines
) where

import           Data.List

data CodeLine = CodeLine {
      number :: Int
    , value :: String
} deriving (Show)

mapCodeLine i v = CodeLine {
        number = i,
        value = v
    }

dropSpaces :: String -> String
dropSpaces = dropWhile (`elem` [' ', '\t'])

isEmptyLine :: String -> Bool
isEmptyLine str = length a == 0
    where a = dropSpaces str

isComment :: String -> Bool
isComment str = a b == '#'
    where a [] = 's'
          a xs = head xs
          b = dropSpaces str

lineFilter l = not a && not b
    where v = value l
          a = isEmptyLine v
          b = isComment v

lineFold acc line = acc ++ a:[]
    where a = mapCodeLine (length acc) line

parseCodeLines :: String -> [CodeLine]
parseCodeLines str = filter lineFilter allLines
    where allLines = foldl lineFold [] $ lines str
