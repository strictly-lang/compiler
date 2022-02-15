module Emitter.Types where

import Control.Monad.State.Lazy (State)
import Types (LeftHandSide)

data Code = Ln String | Ind [Code] | Br

data AppState = AppState
  { componentName :: String,
    expressionIdCounter :: Int
  }

data Variable = DotNotation String | BracketNotation String

type VariableStack = [([Variable], LeftHandSide)]

type AppStateMonad = State AppState