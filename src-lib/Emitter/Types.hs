module Emitter.Types where

import Control.Monad.State.Lazy (State)
import Types (LeftHandSide, Statement, TypeDefinition, UntypedExpression)

data Code = Ln String | Ind [Code] | Br
  deriving (Show)

data AppState = AppState
  { componentName :: String,
    expressionIdCounter :: Int,
    modules :: [(String, [(String, String)])]
  }

data Variable = DotNotation String | BracketNotation String
  deriving (Eq)

data Sibling = SiblingAlways [Variable] | SiblingCondition [Code] [Sibling] [Sibling]

data ViewResult = ViewResult
  { runViewCreate :: [Code],
    runViewUpdate :: [([Variable], [Code])],
    runViewUnmount :: [Code],
    runViewDelete :: [Code],
    runSiblings :: [Sibling]
  }

type Parent = [Variable]

data StackHandler = StackHandler
  { runPrimitive :: AppStateMonad ([[Variable]], [Code]),
    runFunctionApplication :: [StackHandler] -> AppStateMonad StackHandler,
    runProperty :: String -> AppStateMonad StackHandler,
    runViewStream :: [Variable] -> Parent -> [Sibling] -> LeftHandSide -> [Statement] -> AppStateMonad ViewResult,
    runResolvedType :: TypeDefinition,
    runPatternMatching :: LeftHandSide -> AppStateMonad Stack
  }

type StackParameter = Either ([[Variable]], [Code]) [UntypedExpression]

type TypeHandler = Stack -> TypeDefinition -> StackParameter -> Maybe (AppStateMonad StackHandler)

type StackValueContainer = (Stack -> String -> Maybe StackHandler)

data StackEntry = StackValue StackValueContainer | StackType TypeHandler

type Stack = [StackEntry]

type AppStateMonad = State AppState
