module Emitter.Types.RootAssignment where

import Control.Monad.State.Lazy (MonadState (get))
import Data.Char (toUpper)
import Emitter.Types
import Emitter.Types.Expression
import Emitter.Util (pathToComponentName)
import Types

rootAssignment :: String -> Expression -> AppStateMonad [Code]
rootAssignment "main" expression = do
  appState <- get
  let elementName = slashToDash (componentName appState)
      elementClassName = slashToCamelCase (componentName appState)
  return
    [ Ln ("class " ++ elementClassName ++ " extends HtmlElement {"),
      Ind [],
      Ln "}",
      Br,
      Ln ("customElements.define('" ++ elementName ++ "', " ++ elementClassName ++ ");"),
      Br
    ]
rootAssignment name expression = do
  result <- expressionToCode expression
  return ([Ln (name ++ " = ")] ++ result ++ [Br])

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