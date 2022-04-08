module Emitter.Types where

import Control.Monad.State.Lazy (State)
import Types (LeftHandSide, Statement, TypeDefinition)

data Code = Ln String | Ind [Code] | Br
  deriving (Show)

data AppState = AppState
  { componentName :: String,
    expressionIdCounter :: Int
  }

data Variable = DotNotation String | BracketNotation String
  deriving (Eq)

type Sibling = [Variable]

data ViewResult = ViewResult
  { runViewCreate :: [Code],
    runViewUpdate :: [([Variable], Code)],
    runViewUnmount :: [Code],
    runViewDelete :: [Code],
    runSiblings :: [Sibling]
  }

type Parent = [Variable]

data TypedExpression = TypedExpression
  { runPrimitive :: VariableStack -> AppStateMonad [Code],
    runFunctionApplication :: VariableStack -> [TypedExpression] -> [Code],
    runView :: VariableStack -> [([Variable], TypedExpression)] -> [Variable] -> Parent -> [Sibling] -> AppStateMonad ViewResult,
    runProperty :: VariableStack -> String -> AppStateMonad TypedExpression,
    runResolvedType :: VariableStack -> TypeDefinition
  }

type VariableStack = [(String, [Variable], TypedExpression)]

type AppStateMonad = State AppState
