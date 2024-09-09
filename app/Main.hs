{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Arrow (first)
import qualified Data.ByteString as BS
import Data.Function ((&))
import Data.Maybe (mapMaybe)
import Data.Text (Text)

import Control.Applicative (Alternative (..))
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as T
import Fmt
import Xeno.DOM

data GroupHeader = GroupHeader
    { _msgId :: Text
    , _creDtTm :: Text
    , _nbOfTxs :: Int
    , _ctrlSum :: Double
    , _initgPty :: Text
    }
    deriving (Show)

type ParserError = Text

newtype ParsedDocument = ParsedDocument
    { groupHeader :: GroupHeader
    }
    deriving (Show)

newtype Parser a = Parser {runParser :: [Content] -> (Either ParserError a, [Content])}

instance Functor Parser where
    fmap f p = Parser $ first (fmap f) . runParser p

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser (Right x,)
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    Parser ff <*> Parser xx = Parser $ \nodes0 -> case ff nodes0 of
        (Left e, nodes1) -> (Left e, nodes1)
        (Right f, nodes1) -> case xx nodes1 of
            (Left e, nodes2) -> (Left e, nodes2)
            (Right x, nodes2) -> (Right (f x), nodes2)

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (>>=) p1 p2 = Parser $ \cntnts -> case runParser p1 cntnts of
        (Right a, _) -> runParser (p2 a) cntnts
        (Left e, _) -> (Left e, cntnts)

instance MonadFail Parser where
    fail :: String -> Parser a
    fail message = Parser $ \cntnts -> (Left $ T.pack message, cntnts)

instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ const (Left "Hit an empty alternative", [])
    (<|>) :: Parser a -> Parser a -> Parser a
    (<|>) p1 p2 = Parser $ \cntnts -> case runParser p1 cntnts of
        (Right x, cntnts') -> (Right x, cntnts')
        (Left _, _) -> runParser p2 cntnts

evalParser :: Parser a -> [Content] -> Either ParserError a
evalParser p cntnts = fst $ runParser p cntnts

execParser :: Parser a -> [Content] -> [Content]
execParser p cntnts = snd $ runParser p cntnts

pText :: Parser Text
pText = Parser $ \case
    [Text bs] -> (pure $ decodeUtf8 bs, [])
    xs -> (Left $ fmt "Node is not a text node: " +|| xs ||+ "", xs)

findNode :: Text -> [Content] -> Maybe Node
findNode targetName cntnts =
    flip
        mapMaybe
        cntnts
        ( \case
            (Element node) | decodeUtf8 (name node) == targetName -> Just node
            _ -> Nothing
        )
        & \case
            [n] -> Just n
            [] -> Nothing
            _ -> error $ fmtLn "More than one node found of type: " +|| targetName ||+ ". This should never happen"

withNode :: Text -> Parser a -> Parser a
withNode targetName innerParser = Parser $
    \cntnts -> case findNode targetName cntnts of
        (Just innerNode) -> runParser innerParser (contents innerNode)
        Nothing -> (Left $ fmt "Node not found: " +|| targetName ||+ "", cntnts)

withNodeDeep :: [Text] -> Parser a -> Parser a
withNodeDeep (targetName : deepNames) deepParser = withNode targetName $ withNodeDeep deepNames deepParser
withNodeDeep [] deepParser = deepParser

everythingP :: Parser [Content]
everythingP = Parser $ \cntnts -> (Right cntnts, cntnts)

assertP :: Bool -> Text -> Parser ()
assertP True _ = Parser $ \cntnt -> (Right (), cntnt)
assertP False msg = fail (T.unpack msg)

messageParser :: Parser Text
messageParser = do
    message <- pText
    assertP ("T" `T.isPrefixOf` message) "Message doesn't follow format"
    pure message

isXmlDocument :: Parser ()
isXmlDocument = do
    n <- nodeP
    case lookup "xmlns" (attributes n) of
        Nothing -> fail "No xmlns attribute"
        Just bytestringValue -> case decodeUtf8 bytestringValue of
            "urn:iso:std:iso:20022:tech:xsd:pain.001.001.03" -> pure ()
            value -> fail $ fmt "Unknown xmlns value: " +| value |+ ""

satisfyContent :: ([Content] -> Bool) -> Parser ()
satisfyContent predicate = Parser $ \cntnts ->
    if predicate cntnts
        then (Right (), cntnts)
        else (Left "Content doesn't satisfy predicate", cntnts)

nodeP :: Parser Node
nodeP = do
    [Element n] <- everythingP
    pure n

smallTestParser :: Parser (Text, Text)
smallTestParser = do
    isXmlDocument
    withNodeDeep ["Document", "Bla"] $ do
        message <- withNodeDeep ["Yo", "Message"] messageParser
        binaryData <- withNode "Data" pText
        pure (message, binaryData)

main :: IO ()
main = do
    Right root <- parse <$> BS.readFile "small.xml"
    case evalParser smallTestParser [Element root] of
        Right a -> print a
        Left e -> T.putStrLn e