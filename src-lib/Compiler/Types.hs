module Compiler.Types where

import Control.Monad.State.Lazy
import Types

type PublicVariableName = String

type InternalVariableName = [Property]

data Property = DotNotation String | BracketNotation String
  deriving (Eq)

type VariableStack = [([PublicVariableName], InternalVariableName)]

newtype Context = Context (InternalVariableName, VariableStack)

type UpdateCallbacks = [(InternalVariableName, [Indent])]

data CompileResult = CompileResult
  { compileCreate :: [Indent],
    compilePredecessors :: [Predecessor],
    compileUpdate :: UpdateCallbacks,
    compileRemove :: [Indent]
  }

newtype Predecessor = Predecessor String

data Indent = Ln String | Br | Ind [Indent]

type AppState = (String, Int, [Import])

type AppStateMonad = State AppState