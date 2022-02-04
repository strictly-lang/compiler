module Compiler.Types where

import Control.Monad.State.Lazy (State)

data Code = Ln String | Ind [Code] | Br

data AppState = AppState
  { componentName :: String,
    expressionIdCounter :: Int
  }

type AppStateMonad = State AppState