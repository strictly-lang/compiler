module Types where

data Root
  = RootDataDeclaration String [(String, [String])]
  | RootAssignment String Expression
  deriving (Show)

data Statement
  = VariableAssignment LeftHandSide Expression
  | Stream LeftHandSide Expression
  | Expression Expression
  deriving (Show)

data Expression
  = RightHandSideVariable String
  | RightHandSideList [Expression]
  | RightHandSideRecord Record
  | RightHandSideAlgebraicDataType String [Expression]
  | RightHandSideNumber Integer
  | RightHandSideString [RightHandSideString]
  | RightHandSideFunctionDefinition [LeftHandSide] [Statement]
  | RightHandSideFunctionCall [Expression] [Expression]
  | RightHandSideOperator Operator Expression Expression
  | RightHandSideCondition Expression Expression Expression
  | RightHandSideMatch Expression [(LeftHandSide, Expression)]
  | RightHandSideHost String Record Statement
  | RightHandSideFragment [Expression]
  deriving (Show)

type Record = [(String, Maybe String, Expression)]

data RightHandSideString
  = RightHandSideStringStatic String
  | RightHandSideStringDynamic Expression
  deriving (Show)

data LeftHandSide
  = LeftHandSideVariable String
  | LeftHandSideList [LeftHandSide]
  | LeftHandSideRecord [(String, LeftHandSide)]
  | LeftHandSideAlgebraicDataType String [LeftHandSide]
  | LeftHandSIdeAlias String LeftHandSide
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
