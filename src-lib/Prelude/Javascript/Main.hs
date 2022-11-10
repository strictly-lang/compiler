module Prelude.Javascript.Main where

import Parser.Types (AST, ASTRootNode (ASTRootMacro))
import Prelude.Javascript.Util (Code (Br, Ind, Ln), codeToString, removeFileExtension, slashToCamelCase, slashToDash)
import Prelude.Types

webcomponent :: Macro
webcomponent filePath ast = codeToString 0 True (webcomponent' filePath ast ast)

webcomponent' :: String -> AST -> AST -> [Code]
webcomponent' filePath ast [] = []
webcomponent' filePath ast ((ASTRootMacro "webcomponent") : ast') =
  let filePathWithoutExtension = removeFileExtension filePath
   in [ Ln ("class " ++ slashToCamelCase filePathWithoutExtension ++ " extends HTMLElement {"),
        Ind
          [ Ln "constructor() {",
            Ind
              [ Ln "super();",
                Ln "this.properties = {};"
              ],
            Ln "}",
            Br,
            Ln "connectedCallback() {",
            Ind [Ln "this.attachShadow();"],
            Ln "}"
          ],
        Ln "}",
        Br,
        Ln ("customElements.define(\"" ++ slashToDash filePathWithoutExtension ++ "\", " ++ slashToCamelCase filePathWithoutExtension ++ ");"),
        Br
      ]
webcomponent' filePath ast (currentNode : restNodes) = webcomponent' filePath ast restNodes

macros = [webcomponent]