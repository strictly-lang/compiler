module Emitter.Types.View where

import Emitter.Types
import Emitter.Types.Expression (expressionToCode)
import Emitter.Util (getGetFreshExprId, nameToVariable, variableToString)
import Types

data ViewResult = ViewResult
  { compileCreate :: [Code],
    compileUpdate :: [([Variable], [Code])]
  }

render :: [Statement] -> [Variable] -> VariableStack -> AppStateMonad ViewResult
render [] variableStack root = do
  return
    ( ViewResult
        { compileCreate = [],
          compileUpdate = []
        }
    )
render statements variableStack root = do
  (code, nextStatements) <- render' statements variableStack root
  nextRenders <- render nextStatements variableStack root
  return
    ( ViewResult
        { compileCreate = compileCreate code ++ compileCreate nextRenders,
          compileUpdate = compileUpdate code ++ compileUpdate nextRenders
        }
    )

render' :: [Statement] -> [Variable] -> VariableStack -> AppStateMonad (ViewResult, [Statement])
render' ((Expression [RightHandSideHost hostName attributes nestedStatements]) : nextStatements) parent variableStack = do
  exprId <- getGetFreshExprId
  let ele = nameToVariable "ele" exprId
  children <- render nestedStatements ele variableStack

  return
    ( ViewResult
        { compileCreate =
            [ Ln ("const " ++ variableToString ele ++ " = document.createElement(\"" ++ hostName ++ "\")"),
              Br,
              Ln
                (variableToString parent ++ ".appendChild(" ++ variableToString ele ++ ");"),
              Br
            ]
              ++ compileCreate children,
          compileUpdate = compileUpdate children
        },
      nextStatements
    )
render' ((Expression expression) : nextStatements) parent variableStack = do
  exprId <- getGetFreshExprId
  let ele = nameToVariable "text" exprId
  (result, dependencies) <- expressionToCode variableStack expression

  return
    ( ViewResult
        { compileCreate =
            Ln ("const " ++ variableToString ele ++ " = document.createTextNode(") :
            result
              ++ [ Ln ".toString());",
                   Br,
                   Ln (variableToString parent ++ ".appendChild(" ++ variableToString ele ++ ");"),
                   Br
                 ],
          compileUpdate =
            map
              ( \dependency ->
                  ( dependency,
                    Ln (variableToString ele ++ ".textContent = ") :
                    result ++ [Ln ".toString();", Br]
                  )
              )
              dependencies
        },
      nextStatements
    )
render' statement parent variableStack = do
  error (show statement)