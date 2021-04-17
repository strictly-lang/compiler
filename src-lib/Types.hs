module Types where

type Line = Int

type Column = Int

type Option = [String]

type Position = (Line, Column)

type NodeName = String

data Token = Token Position TokenKind

instance Show Token where
  show (Token _ tokenKind) = show tokenKind

data TokenKind
  = Indentation Int
  | Hash
  | Quote
  | Dollar
  | LBrace
  | RBrace
  | Underscore
  | Feed
  | LogicOperator String
  | Identity String

instance Show TokenKind where
  show (Indentation amount) = concat $ replicate amount "\t"
  show Hash = "#"
  show Quote = "\""
  show Dollar = "$"
  show LBrace = "{"
  show RBrace = "}"
  show Underscore = "_"
  show Feed = "<-"
  show (LogicOperator operator) = operator
  show (Identity value) = value

newtype NodeTuple = NodeTuple (NodeName, Line, [Option], [Node NodeTuple])

type ExprId = Int

data Node a = Node ExprId a | SyntaxError String Position Position
  deriving (Show)

type IndentationLevel = Int

type Scanner a = [[Token]] -> IndentationLevel -> ExprId -> ([Node a], ExprId, [[Token]])

data Root = View [Node View] | Model
  deriving (Show)

newtype Expr = Expr String
  deriving (Show)

data View = Host NodeName [Node View] [Option] | StaticText String | DynamicText String | Condition Expr [Node View] [Node View]
  deriving (Show)

type Compiler a = String -> [Node Root] -> Node a -> String
