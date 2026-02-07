module Main where

import System.IO
import System.Environment (getArgs)
import Text.Parsec

import Parser


parserIO :: Either ParseError a -> IO a
parserIO (Left e) = error "parse error"
parserIO (Right a) = return a


main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> error "Please provide input filename"
        (srcName : args) -> do
            fileContents <- readFile srcName
            (decls, _) <- parserIO $ parseModule srcName fileContents initialParserState
            mapM_ (\x -> putStrLn (show x)) decls
