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
  { runPrimitive :: AppStateMonad [Code],
    runFunctionApplication :: [TypedExpression] -> [Code],
    runView :: [([Variable], TypedExpression)] -> [Variable] -> Parent -> [Sibling] -> AppStateMonad ViewResult,
    runProperty :: String -> TypedExpression
  }

type VariableStack = [(String, [Variable], TypedExpression)]

type TypeError = String

type AppStateMonad = State AppState
