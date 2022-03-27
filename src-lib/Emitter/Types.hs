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

data TypedExpression = TypedExpression
  { runType :: VariableStack -> JsType -> [Code],
    runFunctionApplication :: VariableStack -> [TypedExpression] -> [Code]
  }

type VariableStack = [(LeftHandSide, TypedExpression)]

type TypeError = String

type AppStateMonad = State AppState
