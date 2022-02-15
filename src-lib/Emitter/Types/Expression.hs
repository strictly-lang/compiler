module Emitter.Types.Expression where

import Data.List (intersperse)
import Emitter.Types
import Emitter.Util (leftHandSideToCode, variableToString)
import Types

expressionToCode :: VariableStack -> Expression -> AppStateMonad ([Code], [[Variable]])
expressionToCode = expressionToCode' True

expressionToCode' :: Bool -> VariableStack -> Expression -> AppStateMonad ([Code], [[Variable]])
expressionToCode' translateVariableStack variableStack [] = do
  return ([], [])
expressionToCode' translateVariableStack variableStack (expression : restExpressions) = do
  (result, resultDependencies) <- expressionToCode'' translateVariableStack variableStack expression
  if null restExpressions
    then return (result, resultDependencies)
    else do
      (nested, nestedDependencies) <- expressionToCode' False variableStack restExpressions
      return
        ( result ++ [Ln "."] ++ nested,
          [resultDependency ++ nestedDependency | resultDependency <- resultDependencies, nestedDependency <- nestedDependencies]
        )

expressionToCode'' :: Bool -> VariableStack -> Expression' -> AppStateMonad ([Code], [[Variable]])
expressionToCode'' translateVariableStack variableStack (RightHandSideVariable variableName)
  | translateVariableStack = do
    let (variable, constraint) = leftHandSideToCode variableStack variableName
    return ([Ln (variableToString variable)], [variable])
  | not translateVariableStack = do
    return ([Ln variableName], [[DotNotation variableName]])
expressionToCode'' translateVariableStack variableStack (RightHandSideRecord _) = do
  return
    ( [ Ln "{",
        Ln "}"
      ],
      []
    )
expressionToCode'' translateVariableStack variableStack (RightHandSideString strings) = do
  results <- mapM (rightHandSideStringToCode variableStack) strings
  return
    ( Ln "`" :
      concatMap fst results
        ++ [ Ln "`"
           ],
      concatMap snd results
    )
expressionToCode'' translateVariableStack variableStack expression = do
  error (show expression)

rightHandSideStringToCode :: VariableStack -> RightHandSideString -> AppStateMonad ([Code], [[Variable]])
rightHandSideStringToCode variableStack (RightHandSideStringStatic value) = return ([Ln value], [])
rightHandSideStringToCode variableStack (RightHandSideStringDynamic value) = do
  (nested, dependencies) <- expressionToCode variableStack value
  return (Ln "${" : nested ++ [Ln "}"], dependencies)