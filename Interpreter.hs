module Interpreter (evalExpr) where

import Control.Applicative
import Control.Monad.Error

import Parser

evalExpr :: AST -> Either String Double
evalExpr (ASTExpression e) = evalExpr e
evalExpr (ASTNumber n) = return n
evalExpr (ASTPlus l r) = (+) <$> evalExpr l <*> evalExpr r
evalExpr (ASTMinus l r) = (-) <$> evalExpr l <*> evalExpr r
evalExpr (ASTMultiply l r) = (*) <$> evalExpr l <*> evalExpr r
evalExpr (ASTPower l r) = (**) <$> evalExpr l <*> evalExpr r
evalExpr (ASTDivide l r) = do left <- evalExpr l
                              right <- evalExpr r
                              if right == 0
                                 then Left "division by zero"
                                 else return $ left / right
evalExpr (ASTUnaryMinus x) = negate <$> evalExpr x
evalExpr (ASTUnaryPlus x) = evalExpr x
evalExpr _ = Left "incorrect expression"
