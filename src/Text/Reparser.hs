{-# LANGUAGE RankNTypes #-}

module Text.Reparser (
  reparse,
  SyntaxError (..),
  SyntaxErrorKind (..)) where

import Text.Parsec
import Data.Maybe (isJust)
import Control.Monad (msum, join)

type ParsecParser a = forall x . Parsec String x a

data SyntaxError
  = SyntaxError { line :: Int, column :: Int, kind :: SyntaxErrorKind } deriving (Show, Eq)

type Reparser = ParsecParser (Maybe SyntaxError)

data SyntaxErrorKind
  = UnclosedBracket
  | UnclosedParen
  | UnclosedBrace
  | UnopenBracket
  | UnopenParen
  | UnopenBrace
  | BrokenSingleQuote
  | BrokenDoubleQuote
  | UnopenComment
  | UnclosedComment
  | NonAsciiChar
  deriving (Show, Eq)

lparen, rparen,
  rbrace, lbrace,
  lbracket, rbracket,
  singleQuote, doubleQuote,
  notSingleQuote, notDoubleQuote:: ParsecParser Char

lparen = char '('
rparen = char ')'
lbrace = char '{'
rbrace = char '}'
lbracket = char '['
rbracket = char ']'
singleQuote = char '\''
doubleQuote = char '"'
notSingleQuote = noneOf "'"
notDoubleQuote = noneOf "\""


orFail (Right v) = v
orFail (Left e)  = error (show e)

reparse :: String -> Maybe SyntaxError
reparse = orFail . parse program ""

program :: Reparser
program = strip False

strip :: Bool -> Reparser
strip nested = fmap msum $ many (choice choices)
  where choices | nested = commonParsers
                | otherwise = commonParsers ++ withModes unopen

        commonParsers = stream : openQuotation singleQuotationMode : openQuotation doubleQuotationMode : withModes open
        withModes f = map f [bracketMode, parenMode, braceMode]

data Mode = Mode {
  leftParser :: ParsecParser Char,
  rightParser :: ParsecParser Char,
  unopenKind :: SyntaxErrorKind,
  unclosedKind :: SyntaxErrorKind
}

parenMode   = Mode lparen rparen UnopenParen UnclosedParen
bracketMode = Mode lbracket rbracket UnopenBracket UnclosedBracket
braceMode   = Mode lbrace rbrace UnopenBrace UnclosedBrace

data QuotationMode = QuotationMode {
  quotationParser :: ParsecParser Char,
  notQuotationParser :: ParsecParser Char,
  unclosedQuotationKind :: SyntaxErrorKind
}

singleQuotationMode = QuotationMode singleQuote notSingleQuote BrokenSingleQuote
doubleQuotationMode = QuotationMode doubleQuote notDoubleQuote BrokenDoubleQuote

openQuotation :: QuotationMode -> Reparser
openQuotation mode = do
  position <- getPosition
  startQuotation mode
  endQuotation mode position

startQuotation mode = quotationParser mode *> many (notQuotationParser mode)
endQuotation mode position = closedQuotation mode <|> unclosedQuotation mode position

closedQuotation mode = quotationParser mode *> return Nothing
unclosedQuotation mode position = returnSyntaxError position (unclosedQuotationKind mode)

open :: Mode -> Reparser
open mode = do
  position <- getPosition
  nested <- startNesting mode
  if isJust nested
    then return nested
    else endNesting mode position

unopen :: Mode -> Reparser
unopen mode = do
  position <- getPosition
  rightParser mode
  returnSyntaxError position (unopenKind mode)

startNesting mode = leftParser mode *> (fmap join . optionMaybe $ strip True)
endNesting mode position = closed mode <|> unclosed mode position

closed mode = rightParser mode *> return Nothing
unclosed mode position = returnSyntaxError position (unclosedKind mode)

stream :: Reparser
stream = many1 (noneOf "'\"{}[]()") *> return Nothing

returnSyntaxError position = return . Just . SyntaxError (sourceLine position) (sourceColumn position)
