{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Parser (
  ParserError,
  Parser (..),
  isISO20000Document,
  withNodeDeep,
  messageParser,
  withNode,
  textP,
  evalParser,
  execParser,
  lexeme,
  contentsP,
  assertP,
  attr,
  satisfyContent,
  nodeP,
)
where

import Control.Applicative (Alternative (..))
import Control.Arrow (first, (>>>))
import Control.Monad (guard)
import Data.Function ((&))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Encoding as T
import Fmt
import Xeno.DOM

type ParserError = Text

newtype Parser a = Parser {runParser :: [Content] -> (Either ParserError a, [Content])}

instance Functor Parser where
  fmap f p = Parser $ first (fmap f) . runParser p

instance Applicative Parser where
  pure x = Parser (Right x,)
  Parser ff <*> Parser xx = Parser $ \nodes0 -> case ff nodes0 of
    (Left e, nodes1) -> (Left e, nodes1)
    (Right f, nodes1) -> case xx nodes1 of
      (Left e, nodes2) -> (Left e, nodes2)
      (Right x, nodes2) -> (Right (f x), nodes2)

instance Monad Parser where
  (>>=) p1 p2 = Parser $ \cts -> case runParser p1 cts of
    (Right a, _) -> runParser (p2 a) cts
    (Left e, _) -> (Left e, cts)

instance MonadFail Parser where
  fail message = Parser $ \cts -> (Left $ T.pack message, cts)

instance Alternative Parser where
  empty = Parser $ \cts -> (Left "No more parser paths", cts)
  (<|>) p1 p2 = Parser $ \cts -> case runParser p1 cts of
    (Right x, cts') -> (Right x, cts')
    (Left _, _) -> runParser p2 cts

evalParser :: Parser a -> [Content] -> Either ParserError a
evalParser p cts = fst $ runParser p cts

execParser :: Parser a -> [Content] -> [Content]
execParser p cts = snd $ runParser p cts

rawTextP :: Parser Text
rawTextP = Parser $ \case
  [Text bs] -> (pure $ decodeUtf8 bs, [])
  xs -> (Left $ fmt "Node is not a text node: " +|| xs ||+ "", xs)

-- | The Lexeme function takes a parser, and transforms it into a parser that ignores the surrounding whitespace.
lexeme
  :: Parser Text
  -> Parser Text
lexeme p = T.strip <$> p

textP :: Parser Text
textP = lexeme rawTextP

findNode :: Text -> [Content] -> Maybe Node
findNode targetName =
  mapMaybe
    ( \case
        (Element node) | decodeUtf8 (name node) == targetName -> Just node
        _ -> Nothing
    )
    >>> \case
      [n] -> Just n
      [] -> Nothing
      _ -> error $ fmtLn "More than one node found of type: " +|| targetName ||+ ". This should never happen"

matchesTag :: Text -> Content -> Bool
matchesTag targetName (Element node) | decodeUtf8 (name node) == targetName = True
matchesTag _ _ = False

filterNodes :: (Content -> Bool) -> [Content] -> [Content]
filterNodes p cts = filter p cts

withNodeMultiple :: Text -> Parser a -> Parser [a]
withNodeMultiple targetName innerParser =
  Parser $ \cts ->
    filterNodes (matchesTag targetName) cts
      & \filteredCts -> runParser $ innerParser filteredCts

withNode :: Text -> Parser a -> Parser a
withNode targetName innerParser = Parser $
  \cts -> case findNode targetName cts of
    (Just innerNode) -> runParser innerParser (contents innerNode)
    Nothing -> (Left $ fmt "Node not found: " +|| targetName ||+ "", cts)

withNodeDeep :: [Text] -> Parser a -> Parser a
withNodeDeep = flip (foldr withNode)

contentsP :: Parser [Content]
contentsP = Parser $ \cts -> (Right cts, cts)

assertP :: Bool -> Text -> Parser ()
assertP True _ = Parser $ \cts -> (Right (), cts)
assertP False msg = fail (T.unpack msg)

messageParser :: Parser Text
messageParser = do
  message <- textP
  guard ("T" `T.isPrefixOf` message) <|> fail "Message doesn't follow format"
  pure message

attr :: Text -> Parser Text
attr attrName = do
  n <- nodeP
  case lookup (T.encodeUtf8 attrName) (attributes n) of
    Nothing -> fail $ fmt "Attribute not found: " +|| attrName ||+ ""
    Just bytestringValue -> pure $ decodeUtf8 bytestringValue

isISO20000Document :: Parser ()
isISO20000Document = do
  attr "xmlns" >>= \case
    "urn:iso:std:iso:20022:tech:xsd:pain.001.001.03" -> pure ()
    value -> fail $ fmt "Unknown xmlns value: " +| value |+ ""

satisfyContent :: ([Content] -> Bool) -> Parser ()
satisfyContent predicate = Parser $ \cts ->
  if predicate cts
    then (Right (), cts)
    else (Left "Content doesn't satisfy predicate", cts)

nodeP :: Parser Node
nodeP = do
  [Element n] <- contentsP
  pure n
