module Prelude.Javascript.Main where

import Parser.Types
import Prelude.Javascript.Types (JavaScriptTypeHandler (JavaScriptTypeHandler, getDom))
import Prelude.Javascript.Types.Host (javaScriptTypeHandlerHostContainer)
import Prelude.Javascript.Types.String (javaScriptTypeHandlerStringContainer)
import Prelude.Javascript.Util (Code (Br, Ind, Ln), codeToString, removeFileExtension, slashToCamelCase, slashToDash)
import Prelude.Types
import TypeChecker.Main (findTypehandler)

webcomponent :: Macro
webcomponent filePath ast = codeToString 0 True (webcomponent' filePath ast ast)

webcomponent' :: String -> AST -> AST -> [Code]
webcomponent' filePath ast [] = []
webcomponent' filePath ast ((ASTRootNodeGroupedAssignment name (Just "webcomponent") (Just (ASTTypeDeclarationFunction parameterTypes bodyType)) assignments) : ast') =
  let filePathWithoutExtension = removeFileExtension filePath
      result = renderPatterns assignments
   in algeraicDataTypes ast
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
                 Ind (Ln "this.attachShadow({ mode: \"open\" });" : Br : result),
                 Ln "}"
               ],
             Ln "}",
             Br,
             Ln ("customElements.define(\"" ++ slashToDash filePathWithoutExtension ++ "\", " ++ slashToCamelCase filePathWithoutExtension ++ ");"),
             Br
           ]
webcomponent' filePath ast (currentNode : restNodes) = webcomponent' filePath ast restNodes

renderPatterns :: [ASTExpression] -> [Code]
renderPatterns ([ASTExpressionFunctionDeclaration functionParameter body] : restAssignment) = construction body ++ renderPatterns restAssignment
renderPatterns [] = []

construction :: [ASTStatement] -> [Code]
construction ((ASTExpression expression) : restSatements) =
  let Just typeHandler = findTypehandler types expression
   in getDom typeHandler "this.shadowRoot" ++ [Br] ++ construction restSatements
construction [] = []

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

types :: [ASTExpression -> Maybe JavaScriptTypeHandler]
types = [javaScriptTypeHandlerStringContainer, javaScriptTypeHandlerHostContainer]
