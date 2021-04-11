module Compiler.Types.View (compileView) where

import Types

type Content = String

type Predecessor = String

type Parent = String

compileView exprs = compileView' (reverse exprs)

compileView' :: [Expr View] -> Parent -> (Content, String)
compileView' [] _ = ("", "null")
compileView' (Node exprId (Host nodeName children option) : ns) parent =
  let elementVariable = "el" ++ show exprId
   in ( predecessorContent ++ "\
\       const " ++ elementVariable ++ " =  document.createElement(\"" ++ nodeName ++ "\");\n\
\       " ++ parent ++ ".insertBefore(" ++ elementVariable ++ ", " ++ predecessorElement ++ ");\n\
\",
        elementVariable
      )
  where
    (predecessorContent, predecessorElement) = compileView' ns parent
