module Compiler.Types.Root where

import Types
import Compiler.Types.View ( compileView )

compileRoot :: Compiler Root
compileRoot componentName ast (Node exprId (View children)) = "\
\class " ++ componentName ++ " extends HTMLElement {\n\
\    constructor() {\n\
\       super();\n\
\       this._mounted = false;\n\
\    }\n\
\    connectedCallback() {\n\
\       this.attachShadow({mode: 'open'});\n\
\" ++  fst (compileView "this.shadowRoot" (reverse children)) ++ "\n\
\   }\n\
\}\n"
compileRoot _ _ _ = ""