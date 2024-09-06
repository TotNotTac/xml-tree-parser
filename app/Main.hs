{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Arrow (first)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
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

newtype Parser a = Parser {runParser :: Node -> (a, [Node])}

instance Functor Parser where
    fmap f p = Parser $ first f . runParser p

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser $ const (x, [])
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    Parser f <*> p =
        Parser
            ( \inp ->
                let (a, xs) = runParser p inp
                 in (f a, xs)
            )

pName :: Parser Text
pName = Parser $ \x -> (decodeUtf8 . name $ x, [x])

main :: IO ()
main = do
    Right root <- parse <$> BS.readFile "small.xml"
    print $ runParser pName root