module TypeChecker.Types where

import Parser.Types (ASTExpression, ASTExpression', ASTLeftHandSide, ASTTypeDeclaration)

class TypeHandler a where
  destructure :: a -> String

data TypedUsage
  = TypedUsageNone
  | TypedUsageAlgebraicDataType String [TypedUsage]
  | TypedUsageRecord [(String, TypedUsage)]
  | TypedUsageFunction [([TypedUsage], TypedUsage)]

newtype TypedLeftHandSide = TypedLeftHandSide ([ASTTypeDeclaration], ASTLeftHandSide)

data GroupedStatement = TypedVariableAssignment (Maybe ASTTypeDeclaration) [(ASTLeftHandSide, ASTExpression)]
  deriving (Show)