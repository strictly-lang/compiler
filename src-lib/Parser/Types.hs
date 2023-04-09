module Parser.Types where

import Data.Void (Void)
import Text.Megaparsec (Parsec)

type Parser = Parsec Void String

type IndentationLevel = Int

type AST = [ASTStatement]

data ASTTypeDeclaration
  = ASTTypeDeclarationAlgebraicDataType String [ASTTypeDeclaration]
  | ASTTypeDeclarationFunction [ASTTypeDeclaration] ASTTypeDeclaration
  | ASTTypeDeclarationRecord [(String, ASTTypeDeclaration)]
  deriving (Show)

data ASTStatement
  = ASTStatementVariableExpressionAssignment ASTLeftHandSide ASTExpression
  | ASTStatementVariableTypeAssignment String ASTTypeDeclaration
  | ASTStatementAlgebraicDataTypeDeclaration String [(String, [ASTTypeDeclaration])]
  | ASTStatementTypeDeclaration String ASTTypeDeclaration
  | ASTExpression ASTExpression
  | ASTStream ASTLeftHandSide ASTExpression
  deriving (Show)

type GroupedRecordOption = (String, (Maybe ASTTypeDeclaration, [(Maybe String, ASTExpression)]))

type ASTRecord = ([GroupedRecordOption], [ASTStatement])

data ASTString
  = ASTStringStatic String
  | ASTStringDynamic ASTExpression
  deriving (Show)

type Operator = String

type ASTExpression = [ASTExpression']

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
