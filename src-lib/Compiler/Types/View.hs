module Compiler.Types.View (compileView) where

import Types

type Content = String

type Predecessor = String

type Parent = String

compileView :: [Expr View] -> Parent -> (Content, String)
compileView [] _ = ("", "null")
compileView (Node exprId (StaticText textValue) : ns) parent =
  let elementVariable = "el" ++ show exprId
   in ( "\
\       const " ++ elementVariable ++ " =  document.createTextNode(\"" ++ textValue ++ "\");\n\
\       " ++ parent ++ ".appendChild(" ++ elementVariable ++ ");\n\
\\n" ++ successorContent,
        elementVariable
      )
  where
    (successorContent, successorElement) = compileView ns parent
compileView (Node exprId (DynamicText textValue) : ns) parent =
  let elementVariable = "el" ++ show exprId
   in ( "\
\       this." ++ elementVariable ++ " =  document.createTextNode(\"" ++ textValue ++ "\");\n\
\       " ++ parent ++ ".appendChild(this." ++ elementVariable ++ ");\n\
\\n" ++ successorContent,
        elementVariable
      )
  where
    (successorContent, successorElement) = compileView ns parent
compileView (Node exprId (Host nodeName children option) : ns) parent =
  let elementVariable = "el" ++ show exprId
   in ( "\
\       const " ++ elementVariable ++ " =  document.createElement(\"" ++ nodeName ++ "\");\n\
\       " ++ parent ++ ".appendChild(" ++ elementVariable ++ ");\n\
\\n" ++ fst (compileView children elementVariable) ++ successorContent,
        elementVariable
      )
  where
    (successorContent, successorElement) = compileView ns parent
