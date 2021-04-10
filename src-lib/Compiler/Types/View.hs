module Compiler.Types.View where

import Types

compileView :: Compiler View
compileView componentName ast (Node exprId (Host nodeName children option)) = nodeName
compileView _ _ _ = ""