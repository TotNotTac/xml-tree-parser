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

-- import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
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

newtype ParsedDocument = ParsedDocument
    { groupHeader :: GroupHeader
    }
    deriving (Show)

newtype Parser a = Parser {runParser :: [Content] -> (Maybe a, [Content])}

instance Functor Parser where
    fmap f p = Parser $ first (fmap f) . runParser p

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser (Just x,)
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    Parser ff <*> Parser xx = Parser $ \nodes0 -> case ff nodes0 of
        (Nothing, nodes1) -> (Nothing, nodes1)
        (Just f, nodes1) -> case xx nodes1 of
            (Nothing, nodes2) -> (Nothing, nodes2)
            (Just x, nodes2) -> (Just (f x), nodes2)

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (>>=) p1 p2 = Parser $ \cntnts -> case runParser p1 cntnts of
        (Just a, _) -> runParser (p2 a) cntnts
        (Nothing, _) -> (Nothing, cntnts)

evalParser :: Parser a -> [Content] -> Maybe a
evalParser p cntnts = fst $ runParser p cntnts

execParser :: Parser a -> [Content] -> [Content]
execParser p cntnts = snd $ runParser p cntnts

pText :: Parser Text
pText = Parser $ \case
    [Text bs] -> (pure $ decodeUtf8 bs, [])
    xs -> (Nothing, xs)

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
        Nothing -> (Nothing, cntnts)

pEverything :: Parser [Content]
pEverything = Parser $ \cntnts -> (Just cntnts, cntnts)

main :: IO ()
main = do
    Right root <- parse <$> BS.readFile "small.xml"
    print $
        flip evalParser (contents root) $
            withNode "Bla" $ do
                message <- withNode "Yo" $ withNode "Message" pText
                binaryData <- withNode "Data" pText
                pure (message, binaryData)