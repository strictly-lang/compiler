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

data JsType
  = JsBool
  | JsNumber
  | JsFunctionDefinition
  | JsAlgebraicDataTypeDefinition
  | JsAlgebraicDataTypeApplication [TypedExpression]

data ViewResult = ViewResult
  { runViewCreate :: [Code],
    runViewUpdate :: [Code],
    runViewUnmount :: [Code],
    runViewDelete :: [Code]
  }

type Parent = [Variable]

type Predecessor = Maybe [Variable]

data TypedExpression = TypedExpression
  { runType :: JsType -> AppStateMonad [Code],
    runFunctionApplication :: VariableStack -> [TypedExpression] -> [Code],
    runView :: VariableStack -> [([Variable], TypedExpression)] -> Parent -> Predecessor -> AppStateMonad ViewResult,
    runProperty :: String -> TypedExpression
  }

type VariableStack = [(String, [Variable], TypedExpression)]

type TypeError = String

type AppStateMonad = State AppState
