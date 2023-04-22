module TypeChecker.Types where

import Parser.Types (ASTExpression, ASTExpression', ASTLeftHandSide, ASTTypeDeclaration, Operator)
import WebcomponentEmitter.Types (Code)

class TypeHandler a where
  properties :: a -> [TypeHandlerContainer a] -> [(String, a)]
  call :: a -> [TypeHandlerContainer a] -> Stack a -> [a] -> a
  getTypeDeclaration :: a -> ASTTypeDeclaration

type Stack a = [StackEntry a]

type StackEntry a = (String, a)

data TypeHandlerType = TypeHandlerContainerByReference String | TypeHandlerContainerByLiteral [ASTExpression']

type TypeHandlerContainer a = ASTTypeDeclaration -> Either [Code] [ASTExpression'] -> Maybe a

newtype TypedLeftHandSide = TypedLeftHandSide ([ASTTypeDeclaration], ASTLeftHandSide)

data GroupedStatement
  = GroupedStatementVariableAssignment (Maybe ASTTypeDeclaration) [(ASTLeftHandSide, ASTExpression)]
  | GroupedExpression ASTExpression
  deriving (Show)

data TypedStatement a
  = TypedStatementVariableAssignment [(ASTLeftHandSide, [(ASTExpression', a)])]
  | TypedExpression a
  deriving (Show)
