module DataTypes (
      CodeLine(..)
    , makeCodeLine
) where

import           Helper (dropSpaces)

data CodeLine = CodeLine {
      number    :: Int
    , origin    :: String
    , indention :: Int
} deriving (Show)

makeCodeLine i lineStr = CodeLine {
          number = i
        , origin = dropSpaces lineStr
        , indention = a - b
    } where a = length lineStr
            b = length $ dropSpaces lineStr
