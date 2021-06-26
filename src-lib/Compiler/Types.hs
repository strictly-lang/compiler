module Compiler.Types where

import Control.Monad.State.Lazy

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

type AppState = Int

type AppStateMonad = State AppState