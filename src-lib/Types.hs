module Types where

type Line = Int

type Column = Int

type Option = [String]

type Position = (Line, Column)

type NodeName = String

newtype NodeTuple = NodeTuple (NodeName, Line, [Option], [Node NodeTuple])

type ExprId = Int

data Node a = Node ExprId a | SyntaxError String Position Position
  deriving (Show)

type IndentationLevel = Int

type IndentedLine = (Line, IndentationLevel, String)

type Scanner a = [IndentedLine] -> IndentationLevel -> ExprId -> ([Node a], ExprId, [IndentedLine])

data Root = View [Node View] | Model
  deriving (Show)

newtype Expr = Expr String
  deriving (Show)

data View = Host NodeName [Node View] [Option] | StaticText String | DynamicText String | Condition Expr [Node View] [Node View]
  deriving (Show)

type Compiler a = String -> [Node Root] -> Node a -> String
