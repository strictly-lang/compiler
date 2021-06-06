module Compiler.Types where

type PublicVariableName = String

type InternalVariableName = [Property]

data Property = DotNotation String | BracketNotation String
  deriving (Eq)

type VariableStack = [([PublicVariableName], InternalVariableName)]

newtype Context = Context (InternalVariableName, VariableStack)

newtype UpdateCallbacks = UpdateCallbacks [(InternalVariableName, [Indent])]

newtype RemoveCallbacks = RemoveCallbacks [Indent]

newtype Predecessor = Predecessor String

data Indent = Ln String | Br | Ind [Indent]

type ExprId = Int
