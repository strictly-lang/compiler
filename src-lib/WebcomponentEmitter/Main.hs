module WebcomponentEmitter.Main (emit) where

import Control.Monad.State.Lazy (runState)
import Parser.Types (ASTLeftHandSide (ASTLeftHandSideVariable))
import Prelude.Javascript.Types (JavascriptTypeHandler)
import TypeChecker.Types (TypedStatement (TypedStatementVariableAssignment))
import WebcomponentEmitter.Types
import WebcomponentEmitter.Util (codeToString, removeFileExtension, slashToCamelCase, slashToDash)

emit :: String -> [TypedStatement JavascriptTypeHandler] -> Either String String
emit filePath typedStatements =
  let (result, appState) = runState (emitRoot filePath typedStatements) (AppState {runExpressionId = 0})
   in Right (codeToString 0 True result)

emitRoot :: String -> [TypedStatement JavascriptTypeHandler] -> AppStateMonad [Code]
emitRoot filePath ((TypedStatementVariableAssignment assignments) : restStatements) = do
  case assignments of
    (ASTLeftHandSideVariable variableName, _) : _
      | variableName == "main" -> do
          let filePath' = removeFileExtension filePath
              mounted = [DotNotation "this", DotNotation "_mounted"]
              attributeScope = [DotNotation "this", DotNotation "_attributes"]
              popertyScope = [DotNotation "this", DotNotation "_properties"]

          return
            [ Ln ("class " ++ slashToCamelCase filePath' ++ " extends HTMLElement {"),
              Ind
                [ Ln "constructor() {",
                  Ind
                    [ Ln "super();",
                      Br,
                      Ln "this._mounted = false;",
                      Br,
                      Ln "this._properties = {};",
                      Br,
                      Ln "this._attributes = {};"
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
