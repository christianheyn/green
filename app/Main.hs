module Main where

import Printer (prettyPrint)

-- init
-- estimate-repl
-- estimate
-- estimate-export
-- template
-- undo

main :: IO ()
main = do
    putStrLn "Schätze etwas!\n"
    estimation <- getLine
    putStrLn "\n"
    putStrLn $ prettyPrint estimation
    putStrLn "\n"
