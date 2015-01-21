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

type ParseState = Either String (AST, Tokens)
type Tokens = [Token]

syntaxError :: String -> Tokens -> ParseState
syntaxError msg tokens = Left $ msg ++ " near "
                         ++ (unwords $ map show $ take 3 tokens)

matchToken :: Token -> Tokens -> Either String Tokens
matchToken t (t':ts)
  | t == t' = return ts
matchToken t ts = do syntaxError (show t ++ " expected") ts
                     return ts

parse :: Tokens -> ParseState
parse = parseAddExpr

parseAddExpr :: Tokens -> ParseState
parseAddExpr = parseLeftAssocOperator
               [(TPlus, ASTPlus), (TMinus, ASTMinus)]
               parseMultExpr

parseMultExpr :: Tokens -> ParseState
parseMultExpr (TMinus:ts)  = do (x, ts') <- parseMultExpr ts
                                return (ASTUnaryMinus x, ts')
parseMultExpr (TPlus:ts)   = do (x, ts') <- parseMultExpr ts
                                return (ASTUnaryPlus x, ts')
parseMultExpr (TLParen:ts) = do (x, ts') <- parseAddExpr ts
                                ts'' <- matchToken TRParen ts'
                                return (x, ts'')
parseMultExpr ts = parseLeftAssocOperator
                   [(TMultiply, ASTMultiply), (TDivide, ASTDivide)]
                   parsePowerExpr ts

parsePowerExpr :: Tokens -> ParseState
parsePowerExpr = parseRightAssocOperator [(TPower, ASTPower)] parseNumber

parseLeftAssocOperator :: [(Token, AST -> AST -> AST)] ->
                          (Tokens -> ParseState) ->
                          Tokens -> ParseState
parseLeftAssocOperator tokenToAST parseElem tokens = do
    (initial, ts) <- parseElem tokens
    parseTail initial ts
      where parseTail left [] = return (left, [])
            parseTail left (t:ts) =
                let astOp = lookup t tokenToAST
                 in case astOp of Just x -> parseExpr x left ts
                                  Nothing -> return (left, t:ts)
            parseExpr astOp left ts = do
                (right, ts') <- parseElem ts
                let ast = astOp left right
                parseTail ast ts'

parseRightAssocOperator :: [(Token, AST -> AST -> AST)] ->
                           (Tokens -> ParseState) ->
                           Tokens -> ParseState
parseRightAssocOperator tokenToAST parseElem = parseExpr
  where parseExpr ts = do
        (left, ts') <- parseElem ts
        if null ts'
           then return (left, [])
           else let astOp = lookup (head ts') tokenToAST
                 in case astOp of
                         Just x -> do (right, ts'') <- parseExpr $ tail ts'
                                      return (x left right, ts'')
                         Nothing -> return (left, ts')

parseNumber :: Tokens -> ParseState
parseNumber (TNumber n : ts) = return (ASTNumber n, ts)
parseNumber ts = syntaxError "number expected" ts
