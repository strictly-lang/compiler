module TypeChecker.Types where

import Parser.Types (ASTExpression, ASTExpression', ASTLeftHandSide, ASTTypeDeclaration, Operator)

class TypeHandler a where
  destructure :: a -> String

data TypedUsage
  = TypedUsageNone
  | TypedUsageAlgebraicDataType String [TypedUsage]
  | TypedUsageRecord [(String, TypedUsage)]
  | TypedUsageFunction [([TypedUsage], TypedUsage)]

newtype TypedLeftHandSide = TypedLeftHandSide ([ASTTypeDeclaration], ASTLeftHandSide)

data GroupedStatement
  = GroupedStatementVariableAssignment (Maybe ASTTypeDeclaration) [(ASTLeftHandSide, GroupedExpression)]
  | GroupedExpression GroupedExpression
  deriving (Show)

type GroupedExpression = [GroupedExpression']

data GroupedExpression'
  = GroupedExpressionVariable String
  | GroupedExpressionList [GroupedExpression] [GroupedStatement]
  | GroupedExpressionRecord GroupedRecord
  | GroupedExpressionAlgebraicDataType String [GroupedExpression]
  | GroupedExpressionNumber Int
  | GroupedExpressionRange Int (Maybe Int)
  | GroupedExpressionString [GroupedString]
  | GroupedExpressionFunctionDeclaration [ASTLeftHandSide] [GroupedStatement]
  | GroupedExpressionFunctionCall [GroupedExpression]
  | GroupedExpressionOperator Operator GroupedExpression GroupedExpression
  | GroupedExpressionCondition GroupedExpression [GroupedStatement] [GroupedStatement]
  | GroupedExpressionMatch GroupedExpression [(ASTLeftHandSide, [GroupedStatement])]
  | GroupedExpressionHost String GroupedRecord [GroupedStatement]
  | GroupedExpressionFragment [GroupedExpression]
  deriving (Show)

type GroupedRecordOption = (String, (Maybe ASTTypeDeclaration, [(Maybe String, GroupedExpression)]))

type GroupedRecord = ([GroupedRecordOption], [GroupedStatement])

data GroupedString
  = GroupedStringStatic String
  | GroupedStringDynamic GroupedExpression
  deriving (Show)