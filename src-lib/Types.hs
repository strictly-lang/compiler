module Types where

data Root
  = RootDataDeclaration String [DataDeclaration]
  | RootTypeAlias String TypeDefinition
  | RootTypeAssignment String TypeDefinition
  | RootAssignment String Expression
  deriving (Show)

newtype DataDeclaration = DataDeclaration (String, [DataDeclaration])
  deriving (Show)

data TypeDefinition
  = TypeAlgebraicDataType String [TypeDefinition]
  | TypeFunction [TypeDefinition] TypeDefinition
  | TypeRecord [(String, TypeDefinition)]
  | TypeTuple [TypeDefinition]
  | TypeList TypeDefinition
  deriving (Show)

data Statement
  = VariableAssignment LeftHandSide Expression
  | Stream LeftHandSide Expression
  | Expression Expression
  deriving (Show)

type Expression = [Expression']

data Expression'
  = RightHandSideVariable String
  | RightHandSideList [Expression] [Statement]
  | RightHandSideRecord Record
  | RightHandSideAlgebraicDataType String [Expression]
  | RightHandSideNumber Int
  | RightHandSideRange Int (Maybe Int)
  | RightHandSideString [RightHandSideString]
  | RightHandSideFunctionDefinition [LeftHandSide] [Statement]
  | RightHandSideFunctionCall Expression [Expression]
  | RightHandSideOperator Operator Expression Expression
  | RightHandSideCondition Expression [Statement] [Statement]
  | RightHandSideMatch Expression [(LeftHandSide, [Statement])]
  | RightHandSideHost String Record [Statement]
  | RightHandSideFragment [Expression]
  deriving (Show)

type Record = ([(String, RecordValue)], [Statement])

data RecordValue = RecordExpression (Maybe String) Expression | RecordType TypeDefinition
  deriving (Show)

data RightHandSideString
  = RightHandSideStringStatic String
  | RightHandSideStringDynamic Expression
  deriving (Show)

data LeftHandSide
  = LeftHandSideVariable String
  | LeftHandSideList [LeftHandSide] (Maybe LeftHandSide)
  | LeftHandSideRecord [(String, Maybe LeftHandSide)]
  | LeftHandSideAlgebraicDataType String [LeftHandSide]
  | LeftHandSideAlias String LeftHandSide
  | LeftHandSideHole
  deriving (Show)

data Operator
  = Equal
  | Unequal
  | Plus
  | Minus
  | Multiply
  | Division
  | Modulo
  | Concat
  deriving (Show)
