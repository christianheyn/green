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
    | firstChar == '#' = ("COMMENT", "?")
    | firstChar == '@' = resolveLineType firstWord
    | otherwise = ("VALUE", "?")
    where firstChar = head firstWord
          firstWord = head $ words str'
          str' = dropSpaces str

data CodeLine = CodeLine {
    codeLineNumber      :: Int
    , codeLineOrigin    :: String
    , codeLineIndention :: Int
    , codeLineType      :: (LineType, TagName)
} deriving (Show)

makeCodeLine i lineStr = CodeLine {
          codeLineNumber = i + 1
        , codeLineOrigin = dropSpaces lineStr
        , codeLineIndention = a - b
        , codeLineType = getCodeLineType lineStr
    } where a = length lineStr
            b = length $ dropSpaces lineStr

lineFilter l = not a
    where o = codeLineOrigin l
          a = isEmptyLine o

lineFold acc line = acc ++ a:[]
    where a = makeCodeLine (length acc) line

-- export
parseCodeLines :: String -> [CodeLine]
parseCodeLines str = filter lineFilter allLines
    where allLines = foldl lineFold [] $ lines str

data CodeBlock = CodeBlock {
      codeBlockType        :: String
    , codeBlockLineNumbers :: [Int]
    , codeBlockValue       :: String
    , codeBlockChildren    :: [CodeBlock]
}
