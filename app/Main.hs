module Main where

import System.Environment (getProgName, getArgs)
import System.Exit (die)
import GHC.Iface.Ext.Binary (readHieFile, HieFileResult (..))
import GHC.Types.Name.Cache (initNameCache)
import GHC.Iface.Ext.Types (HieFile(..))

defaultProgName :: String
defaultProgName = "dump-hie"

main :: IO ()
main = do
    args <- getArgs
    prog <- getProgName
    let progName = if null prog then defaultProgName else prog
    if length args /= 1 then die $ "Usage: " ++ progName ++ " <hie-path>"
    else do
        let filename = head args
        nameCache <- initNameCache 'A' []
        hieFileResult <- readHieFile nameCache filename
        let hieFile = hie_file_result hieFileResult
        putStrLn $ hie_hs_file hieFile
