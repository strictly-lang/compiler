module Types where

type Line = Int

type Column = Int

type Option = [String]

type Position = (Line, Column)

type NodeName = String

newtype NodeTuple = NodeTuple (NodeName, Line, [Option], [Expr NodeTuple])

type ExprId = Int;

data Expr a = Node ExprId a | SyntaxError String Position Position