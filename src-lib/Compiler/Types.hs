module Compiler.Types where

type PublicVariableName = String

type InternalVariableName = String

type VariableStack = [(PublicVariableName, InternalVariableName)]

newtype Context = Context VariableStack

type UpdateCode = [(InternalVariableName, String)]

data Predecessor = FirstElement | Predecessor String
