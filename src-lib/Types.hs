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

data Root
  = View [ViewContent]
  | Model Name [MergedOption (Bool, RightHandSide)]
  | RootImport Import
  deriving (Show)

newtype Import = Import (String, [String])
  deriving (Show)

data LeftHandSide
  = LeftAlias String LeftHandSide
  | LeftVariable String
  | LeftTuple [LeftHandSide]
  | LeftType String [LeftHandSide]
  | LeftHole
  | LeftRecord [(String, Maybe LeftHandSide)]
  | LeftList [LeftHandSide] (Maybe LeftHandSide)
  deriving (Show)

data Operator = FeedOperator
  deriving (Show)

data RightHandSideOperator
  = Equal
  | Unequal
  | Plus
  | Minus
  | Multiply
  | Division
  | Modulo
  deriving (Show)

data RightHandSideValue
  = Variable [String]
  | Tuple [RightHandSideValue]
  | FunctionCall RightHandSideValue [RightHandSideValue]
  | MixedTextValue [MixedText]
  | Number Integer
  | RightHandSideRecord [(String, RightHandSideValue)] (Maybe RightHandSideValue)
  | RightHandSideList [RightHandSideValue] [ListSourceOrFilter]
  | RightHandSideOperation RightHandSideOperator RightHandSideValue RightHandSideValue
  | RightHandSideType String [RightHandSideValue]
  deriving (Show)

data ListSourceOrFilter
  = ListSource LeftHandSide RightHandSideValue
  | Filter RightHandSideValue
  deriving (Show)

data RightHandSide
  = RightHandSideValue RightHandSideValue
  | FunctionDefinition [LeftHandSide] RightHandSideValue
  deriving (Show)

newtype Expression a = Expression (LeftHandSide, Operator, a)
  deriving (Show)

data ViewContent
  = Host HostElement (Maybe Import)
  | MixedText [MixedText]
  | Condition RightHandSideValue [ViewContent] [ViewContent]
  | Each [Expression RightHandSideValue] [ViewContent] [ViewContent]
  | ViewModel (Expression RightHandSideValue) [ViewContent]
  | Match RightHandSideValue [Case]
  deriving (Show)

newtype HostElement = HostElement (Name, [MergedOption RightHandSide], [ViewContent])
  deriving (Show)

data Case = Case LeftHandSide [ViewContent]
  deriving (Show)

data MixedText
  = StaticText String
  | DynamicText RightHandSideValue
  deriving (Show)

type Parser = Parsec Void String
