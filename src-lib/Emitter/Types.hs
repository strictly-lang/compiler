module Emitter.Types where

import Control.Monad.State.Lazy (State)
import Types (LeftHandSide, Statement, TypeDefinition, UntypedExpression')

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

newtype TypedExpression = TypedExpression StackHandler

data StackHandler = StackHandler
  { runPrimitive :: AppStateMonad ([Variable], [Code]),
    runFunctionApplication :: [TypedExpression] -> ([[Variable]], [Code]),
    runView :: [TypedExpression] -> [Variable] -> Parent -> [Sibling] -> AppStateMonad ViewResult,
    runProperty :: String -> AppStateMonad TypedExpression,
    runResolvedType :: TypeDefinition
  }

type TypeHandler = Stack -> TypeDefinition -> Maybe (UntypedExpression' -> StackHandler)

data StackEntry = StackValue (String, TypedExpression) | StackType TypeHandler

type Stack = [StackEntry]

type AppStateMonad = State AppState
