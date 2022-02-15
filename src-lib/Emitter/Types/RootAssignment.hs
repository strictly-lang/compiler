module Emitter.Types.RootAssignment where

import Control.Monad.State.Lazy (MonadState (get))
import Data.Char (toUpper)
import Data.List (groupBy, isPrefixOf)
import Emitter.Types
import Emitter.Types.Expression (expressionToCode)
import Emitter.Types.View (ViewResult (compileCreate, compileUpdate), render)
import Emitter.Util (getGetFreshExprId, nameToVariable, pathToComponentName, variableToString)
import Types

rootAssignment :: String -> Expression -> AppStateMonad [Code]
rootAssignment "main" [RightHandSideFunctionDefinition [propertiesParam, attributesParam] statements] = do
  appState <- get
  exprId <- getGetFreshExprId
  let elementName = slashToDash (componentName appState)
      elementClassName = slashToCamelCase (componentName appState)
      properties = DotNotation "this" : nameToVariable "_properties" exprId
      mounted = DotNotation "this" : nameToVariable "_mounted" exprId
      variableStack = [(properties, propertiesParam)]

  children <- render statements [DotNotation "this", DotNotation "shadowRoot"] variableStack
  let getProperty = \property -> if properties `isPrefixOf` property then property !! length properties else error ("Missing watcher for: " ++ show property)
  let groupedUpdates = groupBy (\(a, code) (b, _) -> getProperty a == getProperty b) (compileUpdate children)

  return
    [ Ln ("class " ++ elementClassName ++ " extends HTMLElement {"),
      Ind
        ( [ Ln "constructor() {",
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
              ( [ Ln "this.attachShadow({ mode: 'open' });",
                  Br,
                  Ln (variableToString mounted ++ " = true;"),
                  Br
                ]
                  ++ compileCreate children
              ),
            Ln "}",
            Br
          ]
            ++ concat
              [ [Ln ("set " ++ property ++ "(foo) {"), Br, Ind updateCode, Ln "}", Br]
                | update <- groupedUpdates,
                  (variable, updateCode) <- update,
                  let (DotNotation property) = getProperty variable
              ]
        ),
      Ln "}",
      Br,
      Ln ("customElements.define('" ++ elementName ++ "', " ++ elementClassName ++ ");"),
      Br
    ]
rootAssignment name expression = do
  (result, _) <- expressionToCode [] expression
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