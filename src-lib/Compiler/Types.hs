module Compiler.Types where

type PublicVariableName = String

type InternalVariableName = String

type VariableStack = [(PublicVariableName, InternalVariableName)]

type Scope = String

newtype Context = Context (Scope, VariableStack)

newtype UpdateCallbacks = UpdateCallbacks [(InternalVariableName, [Indent])]

newtype RemoveCallbacks = RemoveCallbacks [Indent]

data Predecessor = FirstElement | Predecessor String

data Indent = Ln String | Ind [Indent]
    deriving (Show)