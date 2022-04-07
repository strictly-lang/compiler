module Emitter.Kinds.Root where

import Control.Monad.State.Lazy (get, runState)
-- import Emitter.Kinds.RootAssignment (rootAssignment)

import Data.Char (toUpper)
import Emitter.Kinds.Expression (toTypedExpression)
import Emitter.Kinds.RootDeclaration (algebraicDataTypeConstructor)
import Emitter.Types
import Emitter.Util (getFreshExprId, nameToVariable, variableToString)
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
compileRoot' variableStack ((RootTypeAssignment "main" typeDefinition@(TypeFunction [propertyTypes, attributeTypes] _)) : (RootAssignment "main" untypedExpression) : restRoot) = do
  exprId <- getFreshExprId
  let param = nameToVariable "main" exprId
  (typedRenderFunction, _) <- toTypedExpression variableStack typeDefinition untypedExpression
  -- (typedProperties, _) <- toTypedExpression variableStack propertyTypes [RightHandSideVariable "properties"]
  -- (typedAttributes, _) <- toTypedExpression variableStack attributeTypes [RightHandSideVariable "attributes"]

  appState <- get
  exprId <- getFreshExprId
  let scope = [DotNotation "this"]
  let componentName' = componentName appState
  let unscopedMounted = nameToVariable "mounted" exprId
  let unscopedProperties = nameToVariable "properties" exprId
  let scopedMounted = scope ++ unscopedMounted
  let scopedProperties = scope ++ unscopedProperties

  view <-
    runView
      typedRenderFunction
      []
      scope
      -- [(scopedProperties, typedProperties), ([], typedAttributes)]
      (scope ++ [DotNotation "shadowRoot"])
      []

  next <- compileRoot' variableStack restRoot

  return
    ( [ Ln ("class " ++ slashToCamelCase componentName' ++ " extends HTMLElement {"),
        Ind
          [ Ln (variableToString unscopedMounted ++ " = false;"),
            Br,
            Ln (variableToString unscopedProperties ++ " = {};"),
            Br,
            Ln "connectedCallback() {",
            Ind
              ( [ Ln "this.attachShadow({mode: 'open'});",
                  Br,
                  Ln (variableToString scopedMounted ++ " = true;"),
                  Br
                ]
                  ++ runViewCreate view
              ),
            Br,
            Ln "}"
          ],
        Ln "}",
        Br,
        Br,
        Ln ("customElements.define(\"" ++ slashToDash componentName' ++ "\", " ++ slashToCamelCase componentName' ++ ");"),
        Br
      ]
        ++ next
    )

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

-- Utilities

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