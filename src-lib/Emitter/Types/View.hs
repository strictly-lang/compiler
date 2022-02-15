module Emitter.Types.View where

import Emitter.Types (AppStateMonad, Code (Br, Ln))
import Emitter.Types.Expression (expressionToCode)
import Emitter.Util (Variable, getGetFreshExprId, nameToVariable, variableToString)
import Types

render :: [Statement] -> [Variable] -> AppStateMonad [Code]
render [] root = do
  return []
render statements root = do
  (code, nextStatements) <- render' statements root
  nextRenders <- render nextStatements root
  return (code ++ nextRenders)

render' :: [Statement] -> [Variable] -> AppStateMonad ([Code], [Statement])
render' ((Expression [RightHandSideHost hostName attributes nestedStatements]) : nextStatements) parent = do
  exprId <- getGetFreshExprId
  let ele = nameToVariable "ele" exprId
  children <- render nestedStatements ele

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
render' ((Expression expression) : nextStatements) parent = do
  exprId <- getGetFreshExprId
  let ele = nameToVariable "text" exprId
  result <- expressionToCode expression

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
render' statement parent = do
  error (show statement)