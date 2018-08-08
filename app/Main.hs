module Main where

import Printer (prettyPrint)

-- init
-- estimate-repl
-- estimate
-- estimate-export
-- template
-- undo
-- $ git blame -L 150,+1 -- fileName.js

main :: IO ()
main = do
    putStrLn "Schätze etwas!\n"
    estimation <- getLine
    putStrLn "\n"
    putStrLn $ prettyPrint estimation
    putStrLn "\n"
