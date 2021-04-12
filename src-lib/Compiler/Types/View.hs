module Compiler.Types.View (compileView) where

import Types

import Compiler.Types

import Compiler.Util (publicVariableToInternal)

type Content = String

type Successor = String

type Parent = String

compileView :: [Expr View] -> Context -> Parent -> Predecessor  -> (Content, Successor, UpdateCodes )
compileView [] context _ _= ("", "null", [])
compileView ((Node exprId (StaticText textValue):ns)) context parent predecessor =
  let elementVariable = "el" ++ show exprId
      (successorContent, successorElement, updateCodes) = compileView ns context parent (Predecessor elementVariable)
   in ( "\
\       const " ++ elementVariable ++ " =  document.createTextNode(\"" ++ textValue ++ "\");\n\
\       " ++ appendChild parent predecessor elementVariable ++ "\n\
\\n" ++ successorContent,
        elementVariable,
        updateCodes
      )
compileView (Node exprId (DynamicText variable) : ns) context@(Context variableStack) parent predecessor =
  let elementVariable = "this._el" ++ show exprId
      (successorContent, successorElement, updateCodes) = compileView ns context parent (Predecessor elementVariable)
      internalVariableName = unsafeVariable (publicVariableToInternal variableStack variable)
   in ( "\
\       " ++ elementVariable ++ " =  document.createTextNode(" ++ internalVariableName ++ ");\n\
\       " ++ appendChild parent predecessor elementVariable ++ "\n\
\\n" ++ successorContent,
        elementVariable,
        (internalVariableName, elementVariable ++ ".textContent = " ++ internalVariableName) : updateCodes
      )
compileView (Node exprId (Host nodeName children option) : ns) context parent predecessor =
  let elementVariable = "el" ++ show exprId
      (childrenContent, _, childrenUpdateCodes) = compileView children context elementVariable FirstElement
      (successorContent, successorElement, successorUpdateCodes) = compileView ns context parent (Predecessor elementVariable)
   in ( "\
\       const " ++ elementVariable ++ " =  document.createElement(\"" ++ nodeName ++ "\");\n\
\       " ++ appendChild parent predecessor elementVariable ++ "\n\
\\n" ++ childrenContent ++ successorContent,
        elementVariable,
        childrenUpdateCodes ++ successorUpdateCodes
      )

-- TODO: a compileerror should be thrown instead
unsafeVariable :: Maybe String -> String
unsafeVariable (Just variable) = variable
unsafeVariable Nothing = "Sorry-There is a fuckup"

type Child = String;
appendChild :: Parent -> Predecessor -> Child -> String
appendChild _ (Predecessor predecessor) child = predecessor ++ ".after(" ++ child ++ ");";
appendChild parent FirstElement child = parent ++ ".prepend(" ++  child ++ ");";