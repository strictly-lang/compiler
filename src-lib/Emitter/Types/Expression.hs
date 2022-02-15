module Emitter.Types.Expression where

import Data.List (intersperse)
import Emitter.Types
import Types

expressionToCode :: Expression -> AppStateMonad [Code]
expressionToCode [] = do
  return []
expressionToCode (expression : expressions) = do
  result <- expressionToCode' expression
  next <- expressionToCode expressions
  return
    ( if null next
        then result
        else result ++ [Ln "."] ++ next
    )

expressionToCode' :: Expression' -> AppStateMonad [Code]
expressionToCode' (RightHandSideRecord _) = do
  return
    [ Ln "{",
      Ln "}"
    ]
expressionToCode' (RightHandSideString strings) = do
  results <- mapM rightHandSideStringToCode strings

  return
    ( Ln "`" :
      concat results
        ++ [ Ln "`"
           ]
    )
expressionToCode' expression = do
  error (show expression)

rightHandSideStringToCode :: RightHandSideString -> AppStateMonad [Code]
rightHandSideStringToCode (RightHandSideStringStatic value) = return [Ln value]
rightHandSideStringToCode (RightHandSideStringDynamic value) = do
  nested <- expressionToCode value
  return (Ln "${" : nested ++ [Ln "}"])