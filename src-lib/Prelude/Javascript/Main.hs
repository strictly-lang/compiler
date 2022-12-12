module Prelude.Javascript.Main where

import Control.Monad.State.Lazy (runState)
import Data.List (intercalate)
import Parser.Types
import Prelude.Javascript.ReferenceTypeHandler (referenceTypeHandlerFactory)
import Prelude.Javascript.Types
import Prelude.Javascript.Types.Host (javaScriptTypeHandlerHostContainer)
import Prelude.Javascript.Types.Record (javaScriptTypeHandlerRecordContainer)
import Prelude.Javascript.Types.String (javaScriptTypeHandlerStringContainer)
import Prelude.Javascript.Util (codeToString, getGetFreshExprId, removeFileExtension, render, slashToCamelCase, slashToDash)
import Prelude.Types
import TypeChecker.Main (findTypehandler)
import TypeChecker.Types (TypeHandlerContext (..), TypeValue (TypeValueByReference))

webcomponent :: Macro
webcomponent filePath ast =
  let (result, appState) = runState (webcomponent' filePath ast ast) (AppState {runExpressionId = 0})
   in codeToString 0 True result

webcomponent' :: String -> AST -> AST -> AppStateMonad [Code]
webcomponent' filePath ast [] = do return []
webcomponent' filePath ast ((ASTRootNodeGroupedAssignment name (Just "webcomponent") (Just (ASTTypeDeclarationFunction [propertyTypes, attributeTypes] bodyType)) assignments) : ast') =
  do
    let filePathWithoutExtension = removeFileExtension filePath
    let Just propertiesTypeHandler = findTypehandler (TypeHandlerContext {TypeChecker.Types.runTypes = types}) (Just propertyTypes) (TypeValueByReference (referenceTypeHandlerFactory [Ln "this.properties"]))
    result <- renderPatterns propertiesTypeHandler assignments
    let propertiesScope = "this.properties"
    setters <- case propertyTypes of
      ASTTypeDeclarationRecord records ->
        mapM
          ( \(propertyName, _) -> do
              exprId <- getGetFreshExprId
              let propertyValue = "propertyValue" ++ show exprId

              return
                [ Ln ("set " ++ propertyName ++ "(" ++ propertyValue ++ ") {"),
                  Ind
                    [ Ln (propertiesScope ++ "." ++ propertyName ++ " = " ++ propertyValue ++ ";")
                    ],
                  Ln "}",
                  Br
                ]
          )
          records
      _ -> do return []

    return
      ( algeraicDataTypes ast
          ++ [ Ln ("class " ++ slashToCamelCase filePathWithoutExtension ++ " extends HTMLElement {"),
               Ind
                 ( [ Ln "constructor() {",
                     Ind
                       [ Ln "super();",
                         Br,
                         Ln (propertiesScope ++ " = {};")
                       ],
                     Ln "}",
                     Br,
                     Br,
                     Ln "connectedCallback() {",
                     Ind (Ln "this.attachShadow({ mode: \"open\" });" : Br : create result),
                     Ln "}",
                     Br,
                     Br
                   ]
                     ++ intercalate [Br] setters
                 ),
               Ln "}",
               Br,
               Ln ("customElements.define(\"" ++ slashToDash filePathWithoutExtension ++ "\", " ++ slashToCamelCase filePathWithoutExtension ++ ");"),
               Br
             ]
      )
webcomponent' filePath ast (currentNode : restNodes) = webcomponent' filePath ast restNodes

renderPatterns :: JavaScriptTypeHandler -> [ASTExpression] -> AppStateMonad JavaScriptDomResult
renderPatterns propertiesTypeHandler ([ASTExpressionFunctionDeclaration [propertiesLeftHandSide, attributesLeftHandSide] body] : restAssignment) = do
  parameterTypeHandler <- destructure propertiesTypeHandler (JavaScriptRenderContext {runParent = "this.shadowRoot", Prelude.Javascript.Types.runTypes = types, runStack = []}) propertiesLeftHandSide
  result <- render (JavaScriptRenderContext {runParent = "this.shadowRoot", Prelude.Javascript.Types.runTypes = types, runStack = map fst parameterTypeHandler}) body
  nextResult <- renderPatterns propertiesTypeHandler restAssignment
  return
    ( JavaScriptDomResult
        { create = create result ++ create nextResult,
          update = update result ++ update nextResult,
          dealloc = dealloc result ++ dealloc nextResult,
          delete = delete result ++ delete nextResult
        }
    )
renderPatterns propertiesTypeHandler [] = do return JavaScriptDomResult {create = [], update = [], dealloc = [], delete = []}
renderPatterns propertiesTypeHandler (expression : restExpressions) = error ("For the renderfunction only functions are allowed, not " ++ show expression)

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

types = [javaScriptTypeHandlerStringContainer, javaScriptTypeHandlerRecordContainer, javaScriptTypeHandlerHostContainer]
