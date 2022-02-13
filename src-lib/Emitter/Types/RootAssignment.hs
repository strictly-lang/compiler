module Emitter.Types.RootAssignment where

import Control.Monad.State.Lazy (MonadState (get))
import Data.Char (toUpper)
import Emitter.Types
import Emitter.Types.Expression
import Emitter.Util (Variable (DotNotation), getGetFreshExprId, nameToVariable, pathToComponentName, variableToString)
import Types

rootAssignment :: String -> Expression -> AppStateMonad [Code]
rootAssignment "main" expression = do
  appState <- get
  exprId <- getGetFreshExprId
  let elementName = slashToDash (componentName appState)
      elementClassName = slashToCamelCase (componentName appState)
      properties = DotNotation "this" : nameToVariable "_properties" exprId
      mounted = DotNotation "this" : nameToVariable "_mounted" exprId

  return
    [ Ln ("class " ++ elementClassName ++ " extends HTMLElement {"),
      Ind
        [ Ln "constructor() {",
          Ind
            [ Ln "super();",
              Br,
              Ln (variableToString properties ++ " = {};"),
              Br,
              Ln (variableToString mounted ++ " = false;"),
              Br
            ],
          Ln "}",
          Br,
          Ln "connectedCallback() {",
          Ind
            [ Ln "this.attachShadow({ mode: 'open' });",
              Br,
              Ln (variableToString mounted ++ " = true;"),
              Br
            ],
          Ln "}",
          Br
        ],
      Ln "}",
      Br,
      Ln ("customElements.define('" ++ elementName ++ "', " ++ elementClassName ++ ");"),
      Br
    ]
rootAssignment name expression = do
  result <- expressionToCode expression
  return ([Ln ("const " ++ name ++ " = ")] ++ result ++ [Ln ";", Br])

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