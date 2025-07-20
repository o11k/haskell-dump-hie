{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import System.Environment (getProgName, getArgs)
import System.Exit (die)
import Data.Map (keys, elems, toList)
import GHC.Iface.Ext.Binary (readHieFile, HieFileResult (..))
import GHC.Types.Name.Cache (initNameCache)
import GHC.Iface.Ext.Types (HieFile (..), HieASTs (..), HieAST (sourcedNodeInfo, nodeChildren), NodeInfo (..), SourcedNodeInfo (getSourcedNodeInfo), NodeOrigin (..), NodeAnnotation (nodeAnnotConstr, nodeAnnotType))
import GHC.Data.FastString (unpackFS, LexicalFastString (..), FastString)
import Control.Monad (when, forM_)
import qualified Data.Set as Set
import Data.List (intercalate)

defaultProgName :: String
defaultProgName = "dump-hie"

main :: IO ()
main = do
    args <- getArgs
    prog <- getProgName
    let progName = if null prog then defaultProgName else prog

    when (length args /= 1) $
        die $ "Usage: " ++ progName ++ " <hie-path>"

    let filename = head args
    nameCache <- initNameCache 'A' []
    hieFileResult <- readHieFile nameCache filename
    let hieFile = hie_file_result hieFileResult
    let asts = elems $ getAsts $ hie_asts hieFile

    when (length asts /= 1) $
        die $ "Panic: " ++ show (length asts) ++ " asts"

    let ast = head asts
    printAST ast

printAST :: HieAST a -> IO ()
printAST ast_ =
    printAST' 0 ast_
    where
        printAST' indent ast = do
            let info = getSourcedNodeInfo $ sourcedNodeInfo ast
            let infoStrs = map (uncurry strNodeInfo) $ toList info
            let infoStr = intercalate "," infoStrs
            putStrLn $ replicate indent ' ' ++ infoStr
            forM_ (nodeChildren ast) (printAST' (indent+1))
        strNodeInfo (origin :: NodeOrigin) (info :: NodeInfo a) =
            intercalate "," $ map
                (\(annot :: NodeAnnotation) ->
                    o_str ++ ":"
                    ++ unpackFS (nodeAnnotType annot) ++ ":"
                    ++ unpackFS (nodeAnnotConstr annot)
                )
                annotations
            where
                o_str = case origin of SourceInfo -> "S"; GeneratedInfo -> "G"
                annotations = Set.toList $ nodeAnnotations info