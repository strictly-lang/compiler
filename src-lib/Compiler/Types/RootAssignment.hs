module Compiler.Types.RootAssignment where

import Compiler.Types
import Compiler.Util (pathToComponentName)
import Control.Monad.State.Lazy
import Data.Char (toUpper)
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
  return
    [ Ln (name ++ " = "),
      Ln "mep;",
      Br
    ]

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