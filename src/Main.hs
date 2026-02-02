module Main where

import System.Environment (getArgs)
import Parser


main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> error "Please provide input filename"
        (a:as) -> do
            fileContent <- readFile a
            let fileLines = lines fileContent
                exprs = map (parseWithWhitespace statement) fileLines
            mapM_ (\x -> putStrLn (show x)) exprs
