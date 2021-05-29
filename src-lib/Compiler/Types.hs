module Compiler.Types where

type PublicVariableName = String

type InternalVariableName = String

type VariableStack = [([PublicVariableName], InternalVariableName)]

type Scope = String

newtype Context = Context (Scope, VariableStack)
  deriving (Show)

newtype UpdateCallbacks = UpdateCallbacks [(InternalVariableName, [Indent])]
  deriving (Show)

newtype RemoveCallbacks = RemoveCallbacks [Indent]
  deriving (Show)

newtype Predecessor = Predecessor String
  deriving (Show)

data Indent = Ln String | Br | Ind [Indent]
  deriving (Show)