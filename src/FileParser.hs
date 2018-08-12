module FileParser (
      parseCodeLines
    , CodeLine (..)
    , getCodeLineType
) where

import           Data.List
import           Helper    (dropSpaces, isEmptyLine, isLineComment)

getTagName [] = "?"
getTagName (x:xs) = take i xs
    where i = (length xs) - 2

resolveLineType [] = ("SYNTAX_ERROR", "UnknownTag")
resolveLineType w
    | "{}" `isSuffixOf` w = ("OBJECT", getTagName w)
    | "[]" `isSuffixOf` w = ("LIST", getTagName w)
    | "()" `isSuffixOf` w = ("FUNCTION", getTagName w)
    | otherwise = if l == 0
                    then ("SYNTAX_ERROR", "MissingTagName")
                    else ("PROPERTY", tail w)
    where l = length $ tail w

type LineType = String
type TagName = String

-- export
-- <- (OBJECT | LIST | PROPERTY | VALUE | COMMENT)
getCodeLineType :: String -> (LineType, TagName)
getCodeLineType str
    | head firstWord == '#' = ("COMMENT", "?")
    | head firstWord == '@' = resolveLineType firstWord
    | otherwise = ("VALUE", "?")
    where firstWord = head $ words str'
          str' = dropSpaces str

data CodeLine = CodeLine {
    number    :: Int
    , origin    :: String
    , indention :: Int
    , isType    :: (LineType, TagName)
} deriving (Show)

makeCodeLine i lineStr = CodeLine {
          number = i
        , origin = dropSpaces lineStr
        , indention = a - b
        , isType = getCodeLineType lineStr
    } where a = length lineStr
            b = length $ dropSpaces lineStr

lineFilter l = not a
    where o = origin l
          a = isEmptyLine o

lineFold acc line = acc ++ a:[]
    where a = makeCodeLine (length acc) line

-- export
parseCodeLines :: String -> [CodeLine]
parseCodeLines str = filter lineFilter allLines
    where allLines = foldl lineFold [] $ lines str
