module Types where

import Data.Functor.Identity (Identity)
import Data.Void (Void)
import Text.Megaparsec (Parsec)

type Line = Int

type Column = Int

type Option = (String, RightHandSide)

type Position = (Line, Column)

type Name = String

type IndentationLevel = Int

data Root = View [ViewContent] | Model Name [Option]
  deriving (Show)

data LeftHandSide = LeftVariable (Maybe String) | LeftTuple [LeftHandSide]
  deriving (Show)

data Operator = FeedOperator
  deriving (Show)

data RightHandSide = Variable [String] | Tuple [RightHandSide] | FunctionCall String [RightHandSide] | FunctionDefinition [LeftHandSide] RightHandSide | MixedTextValue [MixedText] | Number Integer
  deriving (Show)

newtype Expression = Expression (LeftHandSide, Operator, RightHandSide)
  deriving (Show)

data ViewContent = Host Name [Option] [ViewContent] | MixedText [MixedText] | Condition RightHandSide [ViewContent] [ViewContent] | Each [Expression] [ViewContent] [ViewContent]
  deriving (Show)

data MixedText = StaticText String | DynamicText RightHandSide
  deriving (Show)

type Compiler a = String -> [Root] -> Root -> String

type Parser = Parsec Void String
