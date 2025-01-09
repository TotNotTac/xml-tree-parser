{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Main where

import qualified Data.ByteString as BS

import Data.Text (Text, isPrefixOf)
import qualified Data.Text.IO as T

import Control.Monad (guard)
import GHC.Base (Alternative ((<|>)))
import Parser
import Xeno.DOM (Content (Element), parse)

data UID
  = StudentNumber Text
  | EmployeeId Text
  deriving (Show)

data Person = Person
  { name :: Text
  , identification :: UID
  , hobbies :: [Text]
  }
  deriving (Show)

parseStudentNumber :: Parser UID
parseStudentNumber = do
  t <- withNode "StudentNumber" textP
  pure $ StudentNumber t

parseEmployeeId :: Parser UID
parseEmployeeId = do
  t <- withNode "EmployeeID" textP
  assertP ("#" `isPrefixOf` t) "EmployeeID must start with a '#'"
  pure $ EmployeeId t

parseHobbies :: Parser [Text]
parseHobbies = withNodeMultiple "Hobby" textP

smallTestParser :: Parser Person
smallTestParser = do
  withNode "Document" $ do
    n <- withNode "Name" textP
    uid <- withNode "Identification" (parseStudentNumber <|> parseEmployeeId)
    h <- parseHobbies

    pure $ Person{name = n, identification = uid, hobbies = h}

main :: IO ()
main = do
  Right root <- parse <$> BS.readFile "small.xml"
  case evalParser smallTestParser [Element root] of
    Right a -> print a
    Left e -> T.putStrLn $ "Error: " <> e
