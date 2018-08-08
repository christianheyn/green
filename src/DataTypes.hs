module DataTypes (
      CodeLine(..)
    , makeCodeLine
) where

import           Helper (dropSpaces, getTagInfo)

data CodeLine = CodeLine {
      number    :: Int
    , origin    :: String
    , indention :: Int
    , tag :: (String, String)
} deriving (Show)

makeCodeLine i lineStr = CodeLine {
          number = i
        , origin = dropSpaces lineStr
        , indention = a - b
        , tag = getTagInfo lineStr
    } where a = length lineStr
            b = length $ dropSpaces lineStr
