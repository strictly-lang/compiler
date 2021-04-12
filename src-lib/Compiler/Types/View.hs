module Compiler.Types.View (compileView) where

import Types

import Compiler.Types

import Compiler.Util (publicVariableToInternal)

type Content = String

type Successor = String

type Parent = String

compileView :: [Expr View] -> Context -> Parent -> (Content, Successor, UpdateCode )
compileView [] context _ = ("", "null", [])
compileView ((Node exprId (StaticText textValue):ns)) context parent =
  let elementVariable = "el" ++ show exprId
      (successorContent, successorElement, updateCode) = compileView ns context parent
   in ( "\
\       const " ++ elementVariable ++ " =  document.createTextNode(\"" ++ textValue ++ "\");\n\
\       " ++ parent ++ ".appendChild(" ++ elementVariable ++ ");\n\
\\n" ++ successorContent,
        elementVariable,
        updateCode
      )
compileView (Node exprId (DynamicText variable) : ns) context@(Context variableStack) parent =
  let (successorContent, successorElement, updateCode) = compileView ns context parent
      elementVariable = "this._el" ++ show exprId
      internalVariableName = unsafeVariable (publicVariableToInternal variableStack variable)
   in ( "\
\       " ++ elementVariable ++ " =  document.createTextNode(" ++ internalVariableName ++ ");\n\
\       " ++ parent ++ ".appendChild(" ++ elementVariable ++ ");\n\
\\n" ++ successorContent,
        elementVariable,
        (internalVariableName, elementVariable ++ ".textContent = " ++ internalVariableName) : updateCode
      )
compileView (Node exprId (Host nodeName children option) : ns) context parent =
  let elementVariable = "el" ++ show exprId
      (childrenContent, _, childrenUpdateCode) = compileView children context elementVariable
      (successorContent, successorElement, successorUpdateCode) = compileView ns context parent
   in ( "\
\       const " ++ elementVariable ++ " =  document.createElement(\"" ++ nodeName ++ "\");\n\
\       " ++ parent ++ ".appendChild(" ++ elementVariable ++ ");\n\
\\n" ++ childrenContent ++ successorContent,
        elementVariable,
        childrenUpdateCode ++ successorUpdateCode
      )

-- TODO: a compileerror should be thrown instead
unsafeVariable :: Maybe String -> String
unsafeVariable (Just variable) = variable
unsafeVariable Nothing = "Sorry-There is a fuckup"
