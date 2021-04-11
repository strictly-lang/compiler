module Compiler.Types.Root where

import Types
import Compiler.Types.View ( compileView )
import Compiler.Util (slashToDash, slashToCamelCase )

compileRoot :: Compiler Root
compileRoot componentPath ast (Node exprId (View children)) = "\
\(() => {\n\
\class " ++ slashToCamelCase componentPath ++ " extends HTMLElement {\n\
\    constructor() {\n\
\       super();\n\
\       this._mounted = false;\n\
\    }\n\
\    connectedCallback() {\n\
\       this._mounted = true;\n\
\       this.attachShadow({mode: 'open'});\n\
\" ++  fst (compileView children "this.shadowRoot") ++ "\n\
\    }\n\
\}\n\n\
\customElements.define(\"" ++ slashToDash componentPath ++ "\", " ++ slashToCamelCase componentPath ++ ");\n\
\})()\n";

compileRoot _ _ _ = ""