module Emitter.Kinds.Root where

import Control.Monad.State.Lazy (runState)
-- import Emitter.Kinds.RootAssignment (rootAssignment)

import Emitter.Kinds.Expression (toTypedExpression)
import Emitter.Kinds.RootDeclaration (algebraicDataTypeConstructor)
import Emitter.Kinds.View (render)
import Emitter.Types
import Emitter.Util (getGetFreshExprId, nameToVariable)
import Types

compileRoot :: String -> [Root] -> String
compileRoot componentName roots =
  let code = compileRoot' [] roots
      (result, _) = runState code (AppState componentName 0)
   in codeToString 0 True result

compileRoot' :: VariableStack -> [Root] -> AppStateMonad [Code]
compileRoot' variableStack [] = do return []
compileRoot' variableStack (RootDataDeclaration _ dataDeclarations : restRoot) = do
  (result, variableStack') <- algebraicDataTypeConstructor dataDeclarations
  next <- compileRoot' (variableStack' ++ variableStack) restRoot

  return (result ++ next)
compileRoot' variableStack ((RootTypeAssignment "main" typeDefinition) : (RootAssignment "main" untypedExpression) : restRoot) = do
  exprId <- getGetFreshExprId
  let param = nameToVariable "main" exprId
  let (typedExpression, _) = toTypedExpression param typeDefinition untypedExpression

  code <- render typedExpression
  next <- compileRoot' variableStack restRoot

  return (code ++ next)

-- compileRoot' (RootAssignment name expression : restRoot) = do
--   result <- rootAssignment name expression
--   next <- compileRoot' restRoot
--   return (result ++ next)

codeToString :: Int -> Bool -> [Code] -> String
codeToString indentationLevel first [] = ""
codeToString indentationLevel first (Ind nestedCode : restCode) =
  "\n" ++ codeToString (indentationLevel + 1) True nestedCode ++ "\n"
    ++ codeToString indentationLevel True restCode
codeToString indentationLevel first (Ln code : restCode)
  | first = replicate indentationLevel '\t' ++ code'
  | otherwise = code'
  where
    code' = code ++ codeToString indentationLevel False restCode
codeToString indentationLevel first (Br : restCode) = '\n' : codeToString indentationLevel True restCode