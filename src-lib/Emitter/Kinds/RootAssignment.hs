module Emitter.Kinds.RootAssignment where

import Control.Monad.State.Lazy (MonadState (get))
import Data.Char (toUpper)
import Data.List (groupBy, isPrefixOf, partition)
import Emitter.Kinds.Expression (expressionToCode)
import Emitter.Kinds.View (Update, ViewResult (compileCreate, compileUpdate), render)
import Emitter.Types
import Emitter.Util (getGetFreshExprId, nameToVariable, pathToComponentName, variableToString)
import Types

rootAssignment :: String -> UntypedExpression -> AppStateMonad [Code]
rootAssignment "main" [RightHandSideFunctionDefinition [propertiesParam, attributesParam] statements] = do
  appState <- get
  exprId <- getGetFreshExprId
  let elementName = slashToDash (componentName appState)
      elementClassName = slashToCamelCase (componentName appState)
      scope = [DotNotation "this"]
      properties = scope ++ nameToVariable "_properties" exprId
      mounted = scope ++ nameToVariable "_mounted" exprId
      variableStack = [(properties, propertiesParam)]

  children <- render statements scope [DotNotation "this", DotNotation "shadowRoot"] variableStack
  let getProperty = \property -> if properties `isPrefixOf` property then property !! length properties else error ("Missing watcher for: " ++ show property)

  (noneProperties, propertySetters) <- getSetters mounted properties (compileUpdate children)
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
            ++ propertySetters
        ),
      Ln "}",
      Br,
      Ln ("customElements.define('" ++ elementName ++ "', " ++ elementClassName ++ ");"),
      Br
    ]
rootAssignment name expression = do
  (result, _) <- expressionToCode [] expression
  return ([Ln ("const " ++ name ++ " = ")] ++ result ++ [Ln ";", Br])

getSetters :: [Variable] -> [Variable] -> [Update] -> AppStateMonad ([Update], [Code])
getSetters mounted propertyPrefix [] = do return ([], [])
getSetters mounted propertyPrefix allUpdates@(currentUpdate@(variable, _) : restUpdates) = do
  let isProperty = propertyPrefix `isPrefixOf` variable
      propertyChain = take (length propertyPrefix + 1) variable
      (updateCodes, restUpdates') =
        if isProperty
          then partition (isPrefixOf propertyChain . fst) allUpdates
          else ([], restUpdates)
  exprId <- getGetFreshExprId
  let DotNotation propertyName = variable !! length propertyPrefix
  let value = nameToVariable "value" exprId

  (noneProperties, nextUpdates) <- getSetters mounted propertyPrefix restUpdates'

  return
    ( if isProperty then noneProperties else currentUpdate : noneProperties,
      [ Ln ("set " ++ propertyName ++ "(" ++ variableToString value ++ ") {"),
        Ind
          ( [ Ln (variableToString propertyChain ++ " = " ++ variableToString value ++ ";"),
              Br
            ]
              ++ if null updateCodes
                then []
                else
                  [ Ln ("if (" ++ variableToString mounted ++ ") {"),
                    Ind (concatMap snd updateCodes),
                    Ln "}",
                    Br
                  ]
          ),
        Ln "}",
        Br
      ]
        ++ nextUpdates
    )

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