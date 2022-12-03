module TypeChecker.Types where

import Parser.Types (ASTExpression', ASTLeftHandSide)

data TypedStatement a = TypedStatementVariableAssignment ASTLeftHandSide (TypedExpression a)

data TypedExpression a = Mep

class TypeHandler a where
  getProperty :: a -> String
