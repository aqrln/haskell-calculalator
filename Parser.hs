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

syntaxError :: String -> Tokens -> Either String a
syntaxError msg tokens = Left $ msg ++ " near "
                         ++ (unwords $ map show $ take 3 tokens)

matchToken :: Token -> Tokens -> Either String Tokens
matchToken t (t':ts)
  | t == t' = return ts
matchToken t ts = do syntaxError (show t ++ " expected") ts
                     return ts

parse :: Tokens -> Either String AST
parse tokens = do
    (expr, rest) <- parseExpression tokens
    if null rest
       then return expr
       else syntaxError "unexpected tokens" rest

parseExpression :: Tokens -> ParseState
parseExpression ts = do
    (expr, ts') <- parseAddExpr ts
    return (ASTExpression expr, ts')

parseAddExpr :: Tokens -> ParseState
parseAddExpr = parseLeftAssocOperator
               [(TPlus, ASTPlus), (TMinus, ASTMinus)]
               parseMultExpr

parseMultExpr :: Tokens -> ParseState
parseMultExpr = parseLeftAssocOperator
                   [(TMultiply, ASTMultiply), (TDivide, ASTDivide)]
                   parsePowerExpr

parsePowerExpr :: Tokens -> ParseState
parsePowerExpr = parseRightAssocOperator [(TPower, ASTPower)] parseAtomExpr

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

parseAtomExpr :: Tokens -> ParseState
parseAtomExpr (TNumber n : ts) = return (ASTNumber n, ts)
parseAtomExpr (TMinus:ts)  = do (x, ts') <- parseAtomExpr ts
                                return (ASTUnaryMinus x, ts')
parseAtomExpr (TPlus:ts)   = do (x, ts') <- parseAtomExpr ts
                                return (ASTUnaryPlus x, ts')
parseAtomExpr (TLParen:ts) = do (x, ts') <- parseAddExpr ts
                                ts'' <- matchToken TRParen ts'
                                return (x, ts'')
parseAtomExpr ts = syntaxError "expression expected" ts
