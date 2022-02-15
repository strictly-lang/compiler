module Emitter.Types.View where

import Emitter.Types
import Emitter.Types.Expression (expressionToCode)
import Emitter.Util (getGetFreshExprId, nameToVariable, variableToString)
import Types

render :: [Statement] -> [Variable] -> VariableStack -> AppStateMonad [Code]
render [] variableStack root = do
  return []
render statements variableStack root = do
  (code, nextStatements) <- render' statements variableStack root
  nextRenders <- render nextStatements variableStack root
  return (code ++ nextRenders)

render' :: [Statement] -> [Variable] -> VariableStack -> AppStateMonad ([Code], [Statement])
render' ((Expression [RightHandSideHost hostName attributes nestedStatements]) : nextStatements) parent variableStack = do
  exprId <- getGetFreshExprId
  let ele = nameToVariable "ele" exprId
  children <- render nestedStatements ele variableStack

  return
    ( [ Ln ("const " ++ variableToString ele ++ " = document.createElement(\"" ++ hostName ++ "\")"),
        Br,
        Ln
          (variableToString parent ++ ".appendChild(" ++ variableToString ele ++ ");"),
        Br
      ]
        ++ children,
      nextStatements
    )
render' ((Expression expression) : nextStatements) parent variableStack = do
  exprId <- getGetFreshExprId
  let ele = nameToVariable "text" exprId
  (result, dependencies) <- expressionToCode variableStack expression

  return
    ( Ln ("const " ++ variableToString ele ++ " = document.createTextNode(") :
      result
        ++ [ Ln ".toString());",
             Br,
             Ln (variableToString parent ++ ".appendChild(" ++ variableToString ele ++ ");"),
             Br
           ],
      nextStatements
    )
render' statement parent variableStack = do
  error (show statement)