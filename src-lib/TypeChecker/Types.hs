module TypeChecker.Types where

import Parser.Types (ASTLeftHandSide)

data TypedStatement a = TypedStatementVariableAssignment ASTLeftHandSide (TypedExpression a)

data TypedExpression a = Mep