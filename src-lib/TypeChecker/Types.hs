module TypeChecker.Types where

import Parser.Types (ASTExpression, ASTExpression', ASTLeftHandSide, ASTTypeDeclaration, Operator)

class TypeHandler a where
  destructure :: a -> String -> a
  call :: a -> [a] -> a

type Stack a = [(String, a)]

type TypeHandlerContainer a = ASTTypeDeclaration -> Maybe a

newtype TypedLeftHandSide = TypedLeftHandSide ([ASTTypeDeclaration], ASTLeftHandSide)

data GroupedStatement
  = GroupedStatementVariableAssignment (Maybe ASTTypeDeclaration) [(ASTLeftHandSide, ASTExpression)]
  | GroupedExpression ASTExpression
  deriving (Show)

data TypedStatement a
  = TypedStatementVariableAssignment [(ASTLeftHandSide, [(ASTExpression', a)])]
  | TypedExpression a
  deriving (Show)
