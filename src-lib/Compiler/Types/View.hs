module Compiler.Types.View (compileView) where

import Compiler.Types
import Compiler.Util ( publicVariableToInternal, indent)
import Types

type Content = [String]

type Successor = String

type Parent = String

compileView :: [Node View] -> Context -> Parent -> Predecessor -> (Content, Successor, UpdateCodes)
compileView [] context _ _ = ([], "null", [])
compileView ((Node exprId (StaticText textValue) : ns)) context@(Context (scope, _)) parent predecessor =
  let elementVariable = scope ++ ".el" ++ show exprId
      (successorContent, successorElement, updateCodes) = compileView ns context parent (Predecessor elementVariable)
   in ( [ elementVariable ++ " =  document.createTextNode(\"" ++ textValue ++ "\");",
          appendChild parent predecessor elementVariable
        ]
          ++ successorContent,
        elementVariable,
        updateCodes
      )
compileView (Node exprId (DynamicText variable) : ns) context@(Context (scope, variableStack)) parent predecessor =
  let elementVariable = scope ++ "._el" ++ show exprId
      (successorContent, successorElement, updateCodes) = compileView ns context parent (Predecessor elementVariable)
      internalVariableName = unsafeVariable (publicVariableToInternal variableStack variable)
   in ( [ elementVariable ++ " =  document.createTextNode(" ++ internalVariableName ++ ");",
          appendChild parent predecessor elementVariable
        ]
          ++ successorContent,
        elementVariable,
        (internalVariableName, elementVariable ++ ".textContent = " ++ internalVariableName ++ ";") : updateCodes
      )
compileView (Node exprId (Host nodeName children option) : ns) context@(Context (scope, _)) parent predecessor =
  let elementVariable = scope ++ ".el" ++ show exprId
      (childrenContent, _, childrenUpdateCodes) = compileView children context elementVariable FirstElement
      (successorContent, successorElement, successorUpdateCodes) = compileView ns context parent (Predecessor elementVariable)
   in ( [ elementVariable ++ " =  document.createElement(\"" ++ nodeName ++ "\");",
          appendChild parent predecessor elementVariable
        ]
          ++ indent childrenContent
          ++ successorContent,
        elementVariable,
        childrenUpdateCodes ++ successorUpdateCodes
      )
compileView (Node exprId (Condition (Expr expr) positiveChildren negativeChildren) : ns) context@(Context (scope, variableStack)) parent predecessor =
  let conditionVariable = scope ++ ".condition" ++ show exprId
      successor = "(" ++ conditionVariable ++ " ? " ++ positiveSuccessor ++ " : " ++ negativeSuccessor ++ ")"
      (positiveChildrenContent, positiveSuccessor, positiveChildrenUpdateCodes) = compileView positiveChildren context parent predecessor
      (negativeChildrenContent, negativeSuccessor, negativeChildrenUpdateCodes) = compileView negativeChildren context parent predecessor
      internalVariableName = unsafeVariable (publicVariableToInternal variableStack expr)
      (successorContent, successorElement, successorUpdateCodes) = compileView ns context parent (Predecessor successor)
   in ( [ conditionVariable ++ " = " ++ internalVariableName ++ ";",
          "if(" ++ conditionVariable ++ ") {"
        ]
          ++ indent positiveChildrenContent
          ++ [ "} else {"
             ]
          ++ indent negativeChildrenContent
          ++ ["}"]
          ++ successorContent,
        successor,
        [(internalVariableName, "")] ++ positiveChildrenUpdateCodes ++ successorUpdateCodes
      )

-- TODO: a compileerror should be thrown instead
unsafeVariable :: Maybe String -> String
unsafeVariable (Just variable) = variable

type Child = String

appendChild :: Parent -> Predecessor -> Child -> String
appendChild _ (Predecessor predecessor) child = predecessor ++ ".after(" ++ child ++ ");"
appendChild parent FirstElement child = parent ++ ".prepend(" ++ child ++ ");"