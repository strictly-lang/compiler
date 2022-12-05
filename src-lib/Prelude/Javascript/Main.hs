module Prelude.Javascript.Main where

import Control.Monad.State.Lazy (runState)
import Parser.Types
import Prelude.Javascript.Types
import Prelude.Javascript.Types.Host (javaScriptTypeHandlerHostContainer)
import Prelude.Javascript.Types.String (javaScriptTypeHandlerStringContainer)
import Prelude.Javascript.Util (codeToString, removeFileExtension, render, slashToCamelCase, slashToDash)
import Prelude.Types
import TypeChecker.Main (findTypehandler)

webcomponent :: Macro
webcomponent filePath ast =
  let (result, appState) = runState (webcomponent' filePath ast ast) (AppState {runExpressionId = 0})
   in codeToString 0 True result

webcomponent' :: String -> AST -> AST -> AppStateMonad [Code]
webcomponent' filePath ast [] = do return []
webcomponent' filePath ast ((ASTRootNodeGroupedAssignment name (Just "webcomponent") (Just (ASTTypeDeclarationFunction parameterTypes bodyType)) assignments) : ast') =
  do
    let filePathWithoutExtension = removeFileExtension filePath
    result <- renderPatterns assignments
    return
      ( algeraicDataTypes ast
          ++ [ Ln ("class " ++ slashToCamelCase filePathWithoutExtension ++ " extends HTMLElement {"),
               Ind
                 [ Ln "constructor() {",
                   Ind
                     [ Ln "super();",
                       Br,
                       Ln "this.properties = {};"
                     ],
                   Ln "}",
                   Br,
                   Ln "connectedCallback() {",
                   Ind (Ln "this.attachShadow({ mode: \"open\" });" : Br : create result),
                   Ln "}"
                 ],
               Ln "}",
               Br,
               Ln ("customElements.define(\"" ++ slashToDash filePathWithoutExtension ++ "\", " ++ slashToCamelCase filePathWithoutExtension ++ ");"),
               Br
             ]
      )
webcomponent' filePath ast (currentNode : restNodes) = webcomponent' filePath ast restNodes

renderPatterns :: [ASTExpression] -> AppStateMonad JavaScriptDomResult
renderPatterns ([ASTExpressionFunctionDeclaration functionParameter body] : restAssignment) = do
  result <- render (JavaScriptRenderContext {runParent = "this.shadowRoot", runTypes = types}) body
  nextResult <- renderPatterns restAssignment
  return
    ( JavaScriptDomResult
        { create = create result ++ create nextResult,
          update = update result ++ update nextResult,
          dealloc = dealloc result ++ dealloc nextResult,
          delete = delete result ++ delete nextResult
        }
    )
renderPatterns [] = do return JavaScriptDomResult {create = [], update = [], dealloc = [], delete = []}

algeraicDataTypes :: AST -> [Code]
algeraicDataTypes [] = []
algeraicDataTypes (ASTRootNodeGroupedAlgebraicDataTypeDeclaration name dataTypes : restNodes) =
  [ Ln ("function " ++ name ++ "(type, ...args) {"),
    Ind
      [ Ln "this._type = type;",
        Br,
        Ln "this._args = args;"
      ],
    Ln "}",
    Br,
    Br
  ]
    ++ algeraicDataTypes restNodes
algeraicDataTypes (_ : restNodes) = algeraicDataTypes restNodes

macros :: [Macro]
macros = [webcomponent]

types = [javaScriptTypeHandlerStringContainer, javaScriptTypeHandlerHostContainer]
