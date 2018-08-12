module Main where
import           FileParser       (parseCodeLines)
import           Printer          (prettyPrint)
import           System.Directory (getCurrentDirectory)
import           System.IO

-- init
-- estimate-repl
-- estimate
-- estimate-export
-- template
-- undo
-- $ git blame -L 150,+1 -- fileName.js

main :: IO ()
main = do
    handle1 <- openFile "./test/test-files/test_1.green" ReadMode
    fileContent <- hGetContents handle1
    print $ parseCodeLines fileContent
    cwd <- getCurrentDirectory
    print cwd
    --
    -- putStrLn "Schätze etwas!\n"
    -- estimation <- getLine
    -- putStrLn "\n"
    -- putStrLn $ prettyPrint estimation
    -- putStrLn "\n"
