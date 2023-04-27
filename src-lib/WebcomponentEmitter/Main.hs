module WebcomponentEmitter.Main (emit) where

import Control.Monad.State.Lazy (runState)
import Parser.Types (ASTLeftHandSide (ASTLeftHandSideVariable), ASTTypeDeclaration (ASTTypeDeclarationAlgebraicDataType, ASTTypeDeclarationFunction))
import Prelude.Javascript.Types
import TypeChecker.Main (findTypeHandler)
import TypeChecker.Types (TypeHandlerContainer, TypedStatement (TypedStatementVariableAssignment))
import WebcomponentEmitter.Types
import WebcomponentEmitter.Util (codeToString, propertyToCode, removeFileExtension, slashToCamelCase, slashToDash)

abortController = [DotNotation "abortController"]

emit :: [TypeHandlerContainer JavascriptTypeHandler] -> String -> [TypedStatement JavascriptTypeHandler] -> Either String String
emit typeHandlerContainers filePath typedStatements =
  let (result, appState) = runState (emitRoot typeHandlerContainers filePath typedStatements) (AppState {runExpressionId = 0})
   in Right (codeToString 0 True result)

emitRoot :: [TypeHandlerContainer JavascriptTypeHandler] -> String -> [TypedStatement JavascriptTypeHandler] -> AppStateMonad [Code]
emitRoot typeHandlerContainers filePath ((TypedStatementVariableAssignment assignments) : restStatements) = do
  case assignments of
    [(ASTLeftHandSideVariable variableName, [(_, mainFunctionHandler)])]
      | variableName == "main" -> do
          let filePath' = removeFileExtension filePath
              mounted = [DotNotation "this", DotNotation "_mounted"]
              attributesScope = [DotNotation "this", DotNotation "_attributes"]
              popertyScope = [DotNotation "this", DotNotation "_properties"]
              mainScope = [DotNotation "this", DotNotation "_main"]
              (ASTTypeDeclarationFunction [propertiesType, attributesType] (ASTTypeDeclarationAlgebraicDataType "Output" [])) = getTypeDeclaration mainFunctionHandler
              propertyHandler = findTypeHandler typeHandlerContainers propertiesType (Left (propertyToCode popertyScope))
              attributeHandler = findTypeHandler typeHandlerContainers attributesType (Left (propertyToCode attributesScope))
              mainFunction =
                call
                  mainFunctionHandler
                  typeHandlerContainers
                  []
                  [propertyHandler, attributeHandler]

          return
            [ Ln ("class " ++ slashToCamelCase filePath' ++ " extends HTMLElement {"),
              Ind
                [ Ln "constructor() {",
                  Ind
                    [ Ln "super();",
                      Br,
                      Ln "this._mounted = false;",
                      Br,
                      Inl (propertyToCode popertyScope),
                      Ln " = {};",
                      Br,
                      Inl (propertyToCode attributesScope),
                      Ln " = {};",
                      Br,
                      Inl (propertyToCode mainScope),
                      Ln " = { ",
                      Inl
                        ( propertyToCode
                            abortController
                        ),
                      Ln ": new AbortController() };"
                    ],
                  Ln "}",
                  Br,
                  Br,
                  Ln "connectedCallback() {",
                  Ind
                    [ Ln "this._mounted = true;",
                      Br,
                      Ln "this.attachShadow({mode: 'open'});"
                    ],
                  Ln "}"
                ],
              Ln "}",
              Br,
              Ln
                ("customElements.define(\"" ++ slashToDash filePath' ++ "\", " ++ slashToCamelCase filePath' ++ ");")
            ]
