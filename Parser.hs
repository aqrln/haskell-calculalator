module Parser ( AST(..)
              , parse
              ) where

import Control.Monad.Error
import Lexer

data AST = ASTNumber Double
         | ASTPlus AST AST
         | ASTMinus AST AST
         | ASTMultiply AST AST
         | ASTDivide AST AST
         | ASTPower AST AST
         | ASTUnaryMinus AST
         | ASTUnaryPlus AST
         | ASTExpression AST
         deriving (Eq, Show)

type MaybeAST = Either String (AST, Tokens)
type Tokens = [Token]

syntaxError :: String -> Tokens -> MaybeAST
syntaxError msg tokens = Left $ msg ++ " near "
                         ++ (unwords $ map show $ take 3 tokens)

matchToken :: Token -> Tokens -> Either String Tokens
matchToken t (t':ts)
  | t == t' = return ts
matchToken t ts = do syntaxError (show t ++ " expected") ts
                     return ts

parse :: Tokens -> MaybeAST
parse = parseExpression

parseExpression :: Tokens -> MaybeAST
parseExpression tokens = do
    (initial, ts) <- parseFactor tokens
    parseTail initial ts
      where parseTail left [] = return (left, [])
            parseTail left (t:ts)
              | t == TPlus = parseExpr ASTPlus left ts
              | t == TMinus = parseExpr ASTMinus left ts
              | otherwise = return (left, t:ts)
            parseExpr astOp left ts = do
                (right, ts') <- parseFactor ts
                let ast = astOp left right
                parseTail ast ts'

parseFactor :: Tokens -> MaybeAST
parseFactor tokens@(TNumber n : ts) = parseNumber tokens
parseFactor (TMinus:ts) = do (x, ts') <- parseFactor ts
                             return (ASTUnaryMinus x, ts')
parseFactor (TPlus:ts) = do (x, ts') <- parseFactor ts
                            return (ASTUnaryPlus x, ts')
parseFactor (TLParen:ts) = do (x, ts') <- parseExpression ts
                              ts'' <- matchToken TRParen ts'
                              return (x, ts'')
parseFactor ts = syntaxError "expression expected" ts

parseNumber :: Tokens -> MaybeAST
parseNumber (TNumber n : ts) = return (ASTNumber n, ts)
parseNumber ts = syntaxError "number expected" ts
