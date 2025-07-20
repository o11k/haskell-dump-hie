module Main where

import System.Environment (getProgName, getArgs)
import System.Exit (die)

defaultProgName :: String
defaultProgName = "dump-hie"

main :: IO ()
main = do
    args <- getArgs
    prog <- getProgName
    let progName = if null prog then defaultProgName else prog
    case args of
        [filename]  -> putStrLn filename
        _           -> die $ "Usage: " ++ progName ++ " <hie-path>"
