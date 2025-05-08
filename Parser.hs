module Parser where

import Control.Applicative
import Data.Char (isAlphaNum, isLower, isSpace, isUpper)
import Debug.Trace

{- The following is the description of CFG that is currently supported.
 - Currently it requires all brackets.

    <term>        ::= <variable>
                    | '\' <variable> '.' <term>    (* abstraction *)
                    | <term> <term>                (* application *)
                    | '(' <term> ')'               (* parenthesis *)
    <variable>    ::= 'a' | 'b' | ... | 'z'
-}

data Term = Abs String Term | App Term Term | Var String deriving (Show)

newtype Parser a = Parser {run :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap f (Parser r) = Parser $ \s -> do
    (val, rest) <- r s
    Just (f val, rest)

instance Applicative Parser where
  pure x = Parser $ \s -> Just (x, s)
  Parser r1 <*> Parser r2 = Parser $ \s -> do
    (f, s') <- r1 s
    (x, s'') <- r2 s'
    Just (f x, s'')

instance Alternative Parser where
  empty = Parser $ const Nothing
  Parser r1 <|> Parser r2 = Parser $ \s -> r1 s <|> r2 s

charPredParser :: (Char -> Bool) -> Parser Char
charPredParser pred = Parser r
  where
    r "" = Nothing
    r (c : rest) = if pred c then Just (c, rest) else Nothing

charParser :: Char -> Parser Char
charParser c = charPredParser (== c)

lowerParser :: Parser Char
lowerParser = charPredParser isLower

variableParser :: Parser String
variableParser = (: []) <$> lowerParser

termParser :: Parser Term
termParser =
  charParser '(' *> termParser <* charParser ')'
    <|> Abs <$> (charParser '\\' *> variableParser <* charParser '.') <*> termParser
    <|> App <$> termParser <*> termParser
    <|> Var <$> variableParser

parse :: String -> Maybe Term
parse s = do
  (parsedTerm, _) <- run termParser s
  return parsedTerm
