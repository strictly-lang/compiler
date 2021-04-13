module Compiler.Types where

type PublicVariableName = String

type InternalVariableName = String

type VariableStack = [(PublicVariableName, InternalVariableName)]

type Scope = String

newtype Context = Context (Scope, VariableStack)

type UpdateCodes = [(InternalVariableName, String)]

data Predecessor = FirstElement | Predecessor String
