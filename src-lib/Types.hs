module Types where

import Data.Functor.Identity (Identity)
import Data.Void (Void)
import Text.Megaparsec (Parsec)

type Line = Int

type Column = Int

type Option = [String]

type Position = (Line, Column)

type NodeName = String

type IndentationLevel = Int

data Root = View [ViewContent] | Model
  deriving (Show)

data LeftHandSide = LeftVariable String | LeftTuple [LeftHandSide]
  deriving (Show)

data Operator = FeedOperator
  deriving (Show)

data RightHandSide = Variable [String] | Tuple [RightHandSide] | FunctionCall String [RightHandSide]
  deriving (Show)

newtype Attribute = Attribute (LeftHandSide, Operator, RightHandSide)
  deriving (Show)

data ViewContent = Host NodeName [ViewContent] [Option] | Text [MixedText] | Condition RightHandSide [ViewContent] [ViewContent] | Each [Attribute] [ViewContent] [ViewContent]
  deriving (Show)

data MixedText = StaticText String | DynamicText RightHandSide
  deriving (Show)

type Compiler a = String -> [Root] -> Root -> String

type Parser = Parsec Void String
