module Types where

data Root
  = RootDataDeclaration String [DataDeclaration]
  | RootTypeAlias String TypeDefinition
  | RootTypeAssignment String TypeDefinition
  | RootAssignment String UntypedExpression
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
  = VariableAssignment LeftHandSide UntypedExpression
  | Stream LeftHandSide UntypedExpression
  | UntypedExpression UntypedExpression
  deriving (Show)

type UntypedExpression = [UntypedExpression']

data UntypedExpression'
  = RightHandSideVariable String
  | RightHandSideList [UntypedExpression] [Statement]
  | RightHandSideRecord Record
  | RightHandSideAlgebraicDataType String [UntypedExpression]
  | RightHandSideNumber Int
  | RightHandSideRange Int (Maybe Int)
  | RightHandSideString [RightHandSideString]
  | RightHandSideFunctionDefinition [LeftHandSide] [Statement]
  | RightHandSideFunctionCall UntypedExpression [UntypedExpression]
  | RightHandSideOperator Operator UntypedExpression UntypedExpression
  | RightHandSideCondition UntypedExpression [Statement] [Statement]
  | RightHandSideMatch UntypedExpression [(LeftHandSide, [Statement])]
  | RightHandSideHost String Record [Statement]
  | RightHandSideFragment [UntypedExpression]
  deriving (Show)

type Record = ([(String, RecordValue)], [Statement])

data RecordValue = RecordExpression (Maybe String) UntypedExpression | RecordType TypeDefinition
  deriving (Show)

data RightHandSideString
  = RightHandSideStringStatic String
  | RightHandSideStringDynamic UntypedExpression
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
