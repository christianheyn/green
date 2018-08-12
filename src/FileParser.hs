module FileParser (
      parseCodeLines
    , CodeLine (..)
) where

import           Data.List
import           DataTypes (CodeLine (..), makeCodeLine)
import           Helper    (dropSpaces, isEmptyLine, isLineComment)

lineFilter l = not a && not b
    where o = origin l
          a = isEmptyLine o
          b = isLineComment o

lineFold acc line = acc ++ a:[]
    where a = makeCodeLine (length acc) line

-- export
parseCodeLines :: String -> [CodeLine]
parseCodeLines str = filter lineFilter allLines
    where allLines = foldl lineFold [] $ lines str
