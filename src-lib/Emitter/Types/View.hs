module Emitter.Types.View where

import Emitter.Types
import Emitter.Types.Expression (expressionToCode)
import Emitter.Util (getGetFreshExprId, nameToVariable, variableToString)
import GHC.Event (FdKey (keyFd))
import Types

type Update = ([Variable], [Code])

data ViewResult = ViewResult
  { compileCreate :: [Code],
    compileUpdate :: [Update]
  }

render :: [Statement] -> [Variable] -> [Variable] -> VariableStack -> AppStateMonad ViewResult
render [] scope parent variableStack = do
  return
    ( ViewResult
        { compileCreate = [],
          compileUpdate = []
        }
    )
render statements scope parent variableStack = do
  (code, nextStatements) <- render' statements scope parent variableStack
  nextRenders <- render nextStatements scope parent variableStack
  return
    ( ViewResult
        { compileCreate = compileCreate code ++ compileCreate nextRenders,
          compileUpdate = compileUpdate code ++ compileUpdate nextRenders
        }
    )

render' :: [Statement] -> [Variable] -> [Variable] -> VariableStack -> AppStateMonad (ViewResult, [Statement])
render' ((Expression [RightHandSideHost hostName attributes nestedStatements]) : nextStatements) scope parent variableStack = do
  exprId <- getGetFreshExprId
  let ele = scope ++ nameToVariable "ele" exprId
  (attributeCodeCreate, attributeCodeUpdate) <- renderAttributes variableStack ele hostName attributes
  children <- render nestedStatements scope ele variableStack

  return
    ( ViewResult
        { compileCreate =
            [ Ln (variableToString ele ++ " = document.createElement(\"" ++ hostName ++ "\");"),
              Br
            ]
              ++ attributeCodeCreate
              ++ [ Ln
                     (variableToString parent ++ ".appendChild(" ++ variableToString ele ++ ");"),
                   Br
                 ]
              ++ compileCreate children,
          compileUpdate = attributeCodeUpdate ++ compileUpdate children
        },
      nextStatements
    )
render' ((Expression expression) : nextStatements) scope parent variableStack = do
  exprId <- getGetFreshExprId
  let ele = scope ++ nameToVariable "text" exprId
  (result, dependencies) <- expressionToCode variableStack expression

  return
    ( ViewResult
        { compileCreate =
            Ln (variableToString ele ++ " = document.createTextNode(") :
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
render' statement scope parent variableStack = do
  error (show statement)

renderAttributes :: VariableStack -> [Variable] -> String -> Record -> AppStateMonad ([Code], [Update])
renderAttributes variableStack element hostElement ([], []) = do return ([], [])
renderAttributes variableStack element hostElement (currentAttribute : nextAttributes, []) = do
  (currentAtributeCreate, currentAttributeUpdate) <- renderAttribute variableStack element hostElement currentAttribute
  (nextAttributesCreate, nextAttributesUpdate) <- renderAttributes variableStack element hostElement (nextAttributes, [])

  return (currentAtributeCreate ++ nextAttributesCreate, currentAttributeUpdate ++ nextAttributesUpdate)

renderAttribute :: VariableStack -> [Variable] -> String -> (String, Maybe String, Expression) -> AppStateMonad ([Code], [Update])
renderAttribute variableStack element hostElement (attributeName, conditionValue, value) = do
  (value, depedencies) <- expressionToCode variableStack value
  let code = [Ln (variableToString element ++ ".setAttribute(\"" ++ attributeName ++ "\", ")] ++ value ++ [Ln ");", Br]
  return
    ( code,
      [ (dependency, code)
        | dependency <- depedencies
      ]
    )