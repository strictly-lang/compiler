module Prelude.Javascript.Main where

import Parser.Types
import Prelude.Javascript.Types (JavaScriptTypeHandler (JavaScriptTypeHandler))
import Prelude.Javascript.Types.String (javaScriptTypeHandlerString)
import Prelude.Javascript.Util (Code (Br, Ind, Ln), codeToString, removeFileExtension, slashToCamelCase, slashToDash)
import Prelude.Types
import TypeChecker.Main (merge)

webcomponent :: Macro
webcomponent filePath ast = codeToString 0 True (webcomponent' filePath ast ast)

webcomponent' :: String -> AST -> AST -> [Code]
webcomponent' filePath ast [] = []
webcomponent' filePath ast ((ASTRootNodeGroupedAssignment name (Just "webcomponent") (Just (ASTTypeDeclarationFunction parameterTypes bodyType)) assignments) : ast') =
  let filePathWithoutExtension = removeFileExtension filePath
      typedAssigmens = map (\[ASTExpressionFunctionDeclaration functionParameter body] -> (functionParameter, merge types body)) assignments
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
                 Ind [Ln "this.attachShadow({ mode: \"open\" });"],
                 Ln "}"
               ],
             Ln "}",
             Br,
             Ln ("customElements.define(\"" ++ slashToDash filePathWithoutExtension ++ "\", " ++ slashToCamelCase filePathWithoutExtension ++ ");"),
             Br
           ]
webcomponent' filePath ast (currentNode : restNodes) = webcomponent' filePath ast restNodes

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

types :: [JavaScriptTypeHandler]
types = [javaScriptTypeHandlerString]
