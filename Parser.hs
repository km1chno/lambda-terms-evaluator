module Parser where

import Control.Applicative
import Data.Char (isAlphaNum, isLower, isSpace, isUpper)
import Debug.Trace

{- The following is the description of CFG that is currently supported.
 - Note that it's important for the grammar to not have any left-recursion.

    <term>        ::= <atom> <term>              (* application *)
                    | <atom>                     (* abstraction, variable or term in paretheses *)

    <atom>        ::= '(' <term> ')'             (* parentheses *)
                    | <variable>                 (* variable *)
                    | '\' <variable> '.' <term>  (* abstraction *)

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

atomParser :: Parser Term
atomParser =
  charParser '(' *> termParser <* charParser ')'
    <|> Var <$> variableParser
    <|> Abs <$> (charParser '\\' *> variableParser <* charParser '.') <*> termParser

buildApplication :: [Term] -> Term
buildApplication (s : t : rest) = foldl App (App s t) rest

termParser :: Parser Term
termParser =
  buildApplication <$> ((:) <$> atomParser <*> ((:) <$> atomParser <*> many atomParser))
    <|> atomParser

parse :: String -> Maybe Term
parse s = do
  (parsedTerm, _) <- run termParser s
  return parsedTerm
