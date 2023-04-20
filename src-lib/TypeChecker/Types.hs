module TypeChecker.Types where

import Parser.Types (ASTExpression, ASTExpression', ASTLeftHandSide, ASTTypeDeclaration, Operator)

class TypeHandler a where
  properties :: a -> [TypeHandlerContainer a] -> [(String, a)]
  call :: a -> [TypeHandlerContainer a] -> Stack a -> [a] -> [ASTExpression'] -> Maybe a

type Stack a = [StackEntry a]

type StackEntry a = (String, a)

data TypeHandlerType = TypeHandlerContainerByReference String | TypeHandlerContainerByLiteral [ASTExpression']

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
