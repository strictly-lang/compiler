module Types where

type Line = Int

type Column = Int

type Option = [String]

type Position = (Line, Column)

type NodeName = String

newtype NodeTuple = NodeTuple (NodeName, Line, [Option], [Expr NodeTuple])

type ExprId = Int

data Expr a = Node ExprId a | SyntaxError String Position Position

type IndentationLevel = Int

type IndentedLine = (Line, IndentationLevel, String)

type Scanner a = [IndentedLine] -> IndentationLevel -> ExprId -> ([Expr a], ExprId, [IndentedLine])

data Root = View [Expr View] | Properties [Properties]

data View = Host NodeName [Expr View] [Option] | StaticText String | DynamicString String

newtype Properties = Property (String, String)

type Compiler a = String -> [Expr Root] -> Expr a -> String
