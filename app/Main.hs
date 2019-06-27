module Main where

import Lib
import System.Environment (getArgs)

main :: IO ()
main = mainWith myFunc
  where
    mainWith func = do
        args <- getArgs
        case args of
            [input] -> do
                isElf <- func input
                case isElf of
                    True -> putStrLn "is a elf file."
                    False -> putStrLn "not a elf file."
            _ -> putStrLn "error: exactly one argument needed"

    myFunc = isElfFile