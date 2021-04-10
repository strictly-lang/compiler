module Compiler.Types.View where

import Types

type Content = String

type Predecessor = String

type Parent = String

compileView :: Parent -> [Expr View] -> (Content, String)
compileView _ [] = ("", "null")
compileView parent (Node exprId (Host nodeName children option) : ns) =
  let elementVariable = "el" ++ show exprId
   in ( predecessorContent ++ "\
\       const " ++ elementVariable ++ " =  document.createElement(\"" ++ nodeName ++ "\");\n\
\       " ++ parent ++ ".insertBefore(" ++ elementVariable ++ ", " ++ predecessorElement ++ ");\n\
\",
        show exprId
      )
  where
    (predecessorContent, predecessorElement) = compileView parent ns
