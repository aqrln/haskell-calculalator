module Lexer ( Token(..)
             , tokenize
             ) where

import Control.Monad

data Token = TNumber Double
           | TPlus
           | TMinus
           | TMultiply
           | TDivide
           | TLParen
           | TRParen
           | TPower
           deriving (Eq, Show)

singleCharTokens = [ ('+', TPlus)
                   , ('-', TMinus)
                   , ('*', TMultiply)
                   , ('/', TDivide)
                   , ('^', TPower)
                   , ('(', TLParen)
                   , (')', TRParen)
                   ]

whitespace = [' ', '\n', '\r']

tokenize :: String -> Either String [Token]
tokenize "" = return []
tokenize (x:xs)
  | x `elem` whitespace = tokenize xs
tokenize s =
    let result = foldl1 (\ l r -> if l == Nothing then r else l) $
                 map ($ s)
                 [trySingleChar, tryNumber]
     in case result of
             Just (token, rest) -> liftM (token:) $ tokenize rest
             Nothing -> Left $ '`' : head s : "' is not a valid character"

trySingleChar :: String -> Maybe (Token, String)
trySingleChar (c:xs) = do
    token <- lookup c singleCharTokens
    return (token, xs)

tryNumber :: String -> Maybe (Token, String)
tryNumber s = case reads s of
                   [(x, rest)] -> Just (TNumber x, rest)
                   _ -> Nothing
