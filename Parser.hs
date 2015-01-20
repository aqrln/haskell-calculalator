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

parse :: Tokens -> MaybeAST
parse = parseExpression

parseExpression :: Tokens -> MaybeAST
parseExpression tokens = do
    (left, ts) <- parseFactor tokens
    if length ts == 0
       then return (left, [])
       else let operations = [(TPlus, ASTPlus), (TMinus, ASTMinus)]
                op = lookup (head ts) operations
             in case op of
                     Just astOp -> do (right, ts') <- parseExpression $ tail ts
                                      return (astOp left right, ts')
                     Nothing -> syntaxError "additive operation expected" ts

parseFactor :: Tokens -> MaybeAST
parseFactor = parseNumber

parseNumber :: Tokens -> MaybeAST
parseNumber (TNumber n : ts) = return (ASTNumber n, ts)
parseNumber ts = syntaxError "number expected" ts
