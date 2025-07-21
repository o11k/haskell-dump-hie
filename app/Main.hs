{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getProgName, getArgs)
import System.Exit (die)
import Data.Map (toList)
import GHC.Iface.Ext.Binary (readHieFile, HieFileResult (..))
import GHC.Types.Name.Cache (initNameCache)
import GHC.Iface.Ext.Types (HieFile (..), HieASTs (..), HieAST (..), NodeInfo (..), SourcedNodeInfo (getSourcedNodeInfo), NodeOrigin (..), NodeAnnotation (nodeAnnotConstr, nodeAnnotType, NodeAnnotation))
import GHC.Data.FastString (unpackFS, LexicalFastString (..))
import Control.Monad (when)
import qualified Data.Set as Set
import Data.Aeson (Value (..), object, (.=), encode, ToJSON)
import qualified Data.Aeson.Key as JKey
import qualified Data.ByteString.Lazy.Char8 as BL
import GHC.Unit.Types
import GHC (ModuleName(..), RealSrcSpan (srcSpanFile), srcSpanStartLine, srcSpanStartCol, srcSpanEndLine, srcSpanEndCol)
import Data.Text (pack, Text)
import qualified  Data.Text.Encoding as TH

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

    BL.putStrLn $ encode $ hieFileToJSON hieFile


hieFileToJSON :: HieFile -> Value
hieFileToJSON HieFile{..} =
    object [
        "path" .= hie_hs_file,
        "module" .= unpackFS modName,
        "types" .= Null,
        "asts" .= object astJson,
        "exports" .= Null,
        "src" .= TH.decodeUtf8 hie_hs_src,
        "entities" .= Null
    ]
    where
        ModuleName modName = moduleName hie_module
        astMap = getAsts hie_asts
        astJson = map (\(path, ast) -> (JKey.fromText (lfsToText path), hieAstToJSON ast)) $ toList astMap

hieAstToJSON :: ToJSON a => HieAST a -> Value
hieAstToJSON Node {..} =
    object [
        "info" .= sInfoJson,
        "span" .= spanToJson nodeSpan,
        "children" .= map hieAstToJSON nodeChildren
    ]
    where
        spanToJson span_ =
            object [
                "path" .= String (pack $ unpackFS $ srcSpanFile span_),
                "loc" .= [
                    [srcSpanStartLine span_, srcSpanStartCol span_],
                    [srcSpanEndLine span_, srcSpanEndCol span_]
                ]
            ]
        sInfoJson = object $ map sourcedInfoToJson $ toList $ getSourcedNodeInfo sourcedNodeInfo
        sourcedInfoToJson (origin, info) =
            let key = case origin of SourceInfo -> "source"; GeneratedInfo -> "generated"
                infoJson = infoToJson info
            in (JKey.fromString key, infoJson)
        infoToJson NodeInfo{..} =
            object [
                "annotations" .= map annotationToJson (Set.toList nodeAnnotations),
                "types" .= nodeType,
                "identifiers" .= Null
            ]
        annotationToJson NodeAnnotation{..} =
             [ pack $ unpackFS nodeAnnotType, pack $ unpackFS nodeAnnotConstr]

lfsToText :: LexicalFastString -> Text
lfsToText (LexicalFastString fs) = pack $ unpackFS fs
