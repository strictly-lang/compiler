module Emitter.Kinds.Root where

import Control.Monad.State.Lazy (get, runState)
-- import Emitter.Kinds.RootAssignment (rootAssignment)

import Data.Char (toUpper)
import Data.List (find)
import Emitter.Kinds.Expression (addToVariableStack, prelude, toTypedExpression, toTypedExpression')
import Emitter.Kinds.RootDeclaration (algebraicDataTypeConstructor)
import Emitter.Types
import Emitter.Util (getFreshExprId, nameToVariable, variableToString)
import Types

compileRoot :: String -> [Root] -> String
compileRoot componentName roots =
  let code = compileRoot' prelude roots
      (result, _) = runState code (AppState componentName 0)
   in codeToString 0 True result

compileRoot' :: Stack -> [Root] -> AppStateMonad [Code]
compileRoot' variableStack [] = do return []
compileRoot' variableStack (RootDataDeclaration _ dataDeclarations : restRoot) = do
  (result, variableStack') <- algebraicDataTypeConstructor dataDeclarations
  next <- compileRoot' (variableStack' ++ variableStack) restRoot

  return (result ++ next)
compileRoot' stack ((RootTypeAssignment name typeDefinition) : (RootAssignment name' untypedExpression) : restRoot) = do
  result@(TypedExpression stackHandler) <- toTypedExpression stack typeDefinition untypedExpression

  (_, code) <- runPrimitive stackHandler
  let stack' = StackValue (name, result) : stack
  next <- compileRoot' stack' restRoot
  return (code ++ next)
compileRoot' variableStack (currentStatement : rest) = error ("not capable of" ++ show currentStatement)

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

-- Utilities

-- typedOrigin :: [Variable] -> TypeDefinition -> TypedExpression
-- typedOrigin scope (TypeRecord records) =
--   let primitive = \variableStack -> do return ([], [Ln (variableToString scope)])
--    in TypedExpression
--         { runPrimitive = primitive,
--           runProperty = \variableStack propertyName ->
--             let property = find (\(heystackPropertyName, typeDefinition) -> heystackPropertyName == propertyName) records
--              in case property of
--                   Just (_, propertyTypeDefinition) -> do
--                     (_, prefix) <- primitive variableStack
--                     toTypedExpression' prefix propertyTypeDefinition [RightHandSideVariable propertyName]
--                   Nothing ->
--                     error ("Could not find property" ++ propertyName)
--         }
-- typedOrigin scope typeDefinition = error (show typeDefinition)