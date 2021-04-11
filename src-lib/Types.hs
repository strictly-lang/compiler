module Types where

type Line = Int

type Column = Int

type Option = [String]

type Position = (Line, Column)

type NodeName = String

newtype NodeTuple = NodeTuple (NodeName, Line, [Option], [Expr NodeTuple])

type ExprId = Int

data Expr a = Node ExprId a | SyntaxError String Position Position
  deriving (Show)

type IndentationLevel = Int

type IndentedLine = (Line, IndentationLevel, String)

type Scanner a = [IndentedLine] -> IndentationLevel -> ExprId -> ([Expr a], ExprId, [IndentedLine])

data Root = View [Expr View] | Properties [Expr Properties]
  deriving (Show)

data View = Host NodeName [Expr View] [Option] | StaticText String | DynamicText String
  deriving (Show)

newtype Properties = Property (String, String)
  deriving (Show)

type Compiler a = String -> [Expr Root] -> Expr a -> String
