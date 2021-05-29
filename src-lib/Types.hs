module Types where

import Data.Functor.Identity (Identity)
import Data.Void (Void)
import Text.Megaparsec (Parsec)

type Line = Int

type Column = Int

type Option a = (String, a)

type Namespace = String

type MergedOption a = (String, [a])

type Position = (Line, Column)

type Name = String

type IndentationLevel = Int

data Root = View [ViewContent] | Model Name [MergedOption RightHandSide]
  deriving (Show)

data LeftHandSide = LeftVariable String | LeftTuple [LeftHandSide] | LeftType String [LeftHandSide] | LeftHole
  deriving (Show)

data Operator = FeedOperator
  deriving (Show)

data RightHandSideOperator = Plus | Minus | Multiply | Division
  deriving (Show)

data RightHandSideValue = Variable [String] | Tuple [RightHandSideValue] | FunctionCall RightHandSideValue [RightHandSideValue] | MixedTextValue [MixedText] | Number Integer | RightHandSideOperation RightHandSideOperator RightHandSideValue RightHandSideValue | RightHandSideType String [RightHandSideValue]
  deriving (Show)

data RightHandSide = RightHandSideValue RightHandSideValue | FunctionDefinition [LeftHandSide] RightHandSideValue
  deriving (Show)

newtype Expression a = Expression (LeftHandSide, Operator, a)
  deriving (Show)

data ViewContent = Host Name [MergedOption RightHandSide] [ViewContent] | MixedText [MixedText] | Condition RightHandSideValue [ViewContent] [ViewContent] | Each [Expression RightHandSideValue] [ViewContent] [ViewContent] | ViewModel (Expression RightHandSideValue) [ViewContent] | Match RightHandSideValue [Case]
  deriving (Show)

data Case = Case LeftHandSide [ViewContent]
  deriving (Show)

data MixedText = StaticText String | DynamicText RightHandSideValue
  deriving (Show)

type Compiler a = String -> [Root] -> Root -> String

type Parser = Parsec Void String
