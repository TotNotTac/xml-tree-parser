{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.ByteString as BS
import Data.Text (Text)

import Control.Applicative (optional)
import qualified Data.Text.IO as T
import Parser
import Xeno.DOM (Content (Element), parse)

data GroupHeader = GroupHeader
    { _msgId :: Text
    , _creDtTm :: Text
    , _nbOfTxs :: Int
    , _ctrlSum :: Double
    , _initgPty :: Text
    }
    deriving (Show)

newtype ParsedDocument = ParsedDocument
    { groupHeader :: GroupHeader
    }
    deriving (Show)

smallTestParser :: Parser (Text, Text)
smallTestParser = do
    isXmlDocument
    withNodeDeep ["Document", "Bla"] $ do
        message <- withNodeDeep ["Yo", "Message"] messageParser
        binaryData <- withNode "Data" textP
        pure (message, binaryData)

main :: IO ()
main = do
    Right root <- parse <$> BS.readFile "small.xml"
    case evalParser smallTestParser [Element root] of
        Right a -> print a
        Left e -> T.putStrLn e