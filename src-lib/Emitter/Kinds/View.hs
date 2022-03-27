module Emitter.Kinds.View where

import Control.Monad.State.Lazy (MonadState (get), get)
import Data.Char (toUpper)
import Emitter.Types
import Emitter.Util (getGetFreshExprId, nameToVariable, pathToComponentName, variableToString)

render :: TypedExpression -> AppStateMonad [Code]
render typedExpression = do
  appState <- get
  exprId <- getGetFreshExprId
  let componentName' = componentName appState
  let unscopedMounted = nameToVariable "mounted" exprId
  let unscopedProperties = nameToVariable "properties" exprId
  let scopedMounted = DotNotation "this" : unscopedMounted
  let scopedProperties = DotNotation "this" : unscopedProperties

  return
    [ Ln ("class " ++ slashToCamelCase componentName' ++ " extends HTMLElement {"),
      Ind
        [ Ln (variableToString unscopedMounted ++ " = false;"),
          Br,
          Ln (variableToString unscopedProperties ++ " = {};"),
          Br,
          Ln "connectedCallback() {",
          Ind
            [ Ln "this.attachShadow({mode: 'open'});",
              Br,
              Ln (variableToString scopedMounted ++ " = true;")
            ],
          Br,
          Ln "}"
        ],
      Ln "}",
      Br,
      Br,
      Ln ("customElements.define(\"" ++ slashToDash componentName' ++ "\", " ++ slashToCamelCase componentName' ++ ");"),
      Br
    ]

-- Utilities

slashToDash :: String -> String
slashToDash [] = []
slashToDash ('/' : ps) = '-' : slashToDash ps
slashToDash (p : ps) = p : slashToDash ps

slashToCamelCase :: String -> String
slashToCamelCase (p : ps) = toUpper p : slashToCamelCase' ps

slashToCamelCase' :: String -> String
slashToCamelCase' [] = []
slashToCamelCase' ('/' : p : ps) = toUpper p : slashToCamelCase' ps
slashToCamelCase' (p : ps) = p : slashToCamelCase' ps