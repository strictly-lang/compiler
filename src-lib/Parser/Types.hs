module Parser.Types where

import Data.Void (Void)
import Text.Megaparsec (Parsec)

type Parser = Parsec Void String

type IndentationLevel = Int

type AST = [ASTRootNode]

data ASTRootNode
  = ASTRootNodeAlgebraicDataTypeDeclaration String [(String, [ASTTypeDeclaration])]
  | ASTMacro String
  | ASTRootTypeAssignment String ASTTypeDeclaration
  | ASTRootAssignment String ASTExpression
  deriving (Show)

data ASTTypeDeclaration
  = ASTTypeDeclarationList ASTTypeDeclaration
  | ASTTypeDeclarationAlgebraicDataType String [ASTTypeDeclaration]
  | ASTTypeDeclarationFunction [ASTTypeDeclaration] ASTTypeDeclaration
  | ASTTypeDeclarationRecord [(String, ASTTypeDeclaration)]
  deriving (Show)

data ASTStatement
  = ASTStatementVariableAssignment ASTLeftHandSide ASTExpression
  | ASTExpression ASTExpression
  | ASTStream ASTLeftHandSide ASTExpression
  deriving (Show)

type ASTRecord = ([(String, ASTRecordValue)], [ASTStatement])

data ASTRecordValue = RecordExpression (Maybe String) ASTExpression | RecordType ASTTypeDeclaration
  deriving (Show)

data ASTString
  = ASTStringStatic String
  | ASTStringDynamic ASTExpression
  deriving (Show)

type ASTExpression = [ASTExpression']

type Operator = String

data ASTExpression'
  = ASTExpressionVariable String
  | ASTExpressionList [ASTExpression] [ASTStatement]
  | ASTExpressionRecord ASTRecord
  | ASTExpressionAlgebraicDataType String [ASTExpression]
  | ASTExpressionNumber Int
  | ASTExpressionRange Int (Maybe Int)
  | ASTExpressionString [ASTString]
  | ASTExpressionFunctionDeclaration [ASTLeftHandSide] [ASTStatement]
  | ASTExpressionFunctionCall [ASTExpression]
  | ASTExpressionOperator Operator ASTExpression ASTExpression
  | ASTExpressionCondition ASTExpression [ASTStatement] [ASTStatement]
  | ASTExpressionMatch ASTExpression [(ASTLeftHandSide, [ASTStatement])]
  | ASTExpressionHost String ASTRecord [ASTStatement]
  | ASTExpressionFragment [ASTExpression]
  deriving (Show)

data ASTLeftHandSide
  = ASTLeftHandSideVariable String
  | ASTLeftHandSideList [ASTLeftHandSide]
  | ASTLeftHandSideRecord [(String, Maybe ASTLeftHandSide)]
  | ASTLeftHandSideAlgebraicDataType String [ASTLeftHandSide]
  | ASTLeftHandSideAlias String ASTLeftHandSide
  | ASTLeftHandSideHole
  deriving (Show)
