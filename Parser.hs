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
parse = parseExpression

parseExpression :: Tokens -> ParseState
parseExpression = parseLeftAssocOperator
                  [(TPlus, ASTPlus), (TMinus, ASTMinus)]
                  parseFactor

parseFactor :: Tokens -> ParseState
parseFactor (TMinus:ts) = do (x, ts') <- parseFactor ts
                             return (ASTUnaryMinus x, ts')
parseFactor (TPlus:ts) = do (x, ts') <- parseFactor ts
                            return (ASTUnaryPlus x, ts')
parseFactor (TLParen:ts) = do (x, ts') <- parseExpression ts
                              ts'' <- matchToken TRParen ts'
                              return (x, ts'')
parseFactor ts = parseLeftAssocOperator
                 [(TMultiply, ASTMultiply), (TDivide, ASTDivide)]
                 parseNumber ts

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

parseNumber :: Tokens -> ParseState
parseNumber (TNumber n : ts) = return (ASTNumber n, ts)
parseNumber ts = syntaxError "number expected" ts
