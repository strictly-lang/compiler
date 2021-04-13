module Compiler.Types.View (compileView) where

import Compiler.Types
import Compiler.Util ( publicVariableToInternal, indent)
import Types

type Content = [String]

type Successor = String

type Parent = String

compileView :: [Node View] -> Context -> Parent -> Predecessor -> (Content, Successor, UpdateCallbacks)
compileView [] context _ _ = ([], "null", UpdateCallbacks [])
compileView ((Node exprId (StaticText textValue) : ns)) context@(Context (scope, _)) parent predecessor =
  let elementVariable = scope ++ ".el" ++ show exprId
      (successorContent, successorElement, updateCallbacks) = compileView ns context parent (Predecessor elementVariable)
   in ( [ elementVariable ++ " =  document.createTextNode(\"" ++ textValue ++ "\");",
          appendChild parent predecessor elementVariable
        ]
          ++ successorContent,
        elementVariable,
        updateCallbacks
      )
compileView (Node exprId (DynamicText variable) : ns) context@(Context (scope, variableStack)) parent predecessor =
  let elementVariable = scope ++ "._el" ++ show exprId
      (successorContent, successorElement, UpdateCallbacks updateCallbacks) = compileView ns context parent (Predecessor elementVariable)
      internalVariableName = unsafeVariable (publicVariableToInternal variableStack variable)
   in ( [ elementVariable ++ " =  document.createTextNode(" ++ internalVariableName ++ ");",
          appendChild parent predecessor elementVariable
        ]
          ++ successorContent,
        elementVariable,
        UpdateCallbacks ((internalVariableName, elementVariable ++ ".textContent = " ++ internalVariableName ++ ";") : updateCallbacks)
      )
compileView (Node exprId (Host nodeName children option) : ns) context@(Context (scope, _)) parent predecessor =
  let elementVariable = scope ++ ".el" ++ show exprId
      (childrenContent, _, UpdateCallbacks childrenUpdateCallbacks) = compileView children context elementVariable FirstElement
      (successorContent, successorElement, UpdateCallbacks successorUpdateCallbacks) = compileView ns context parent (Predecessor elementVariable)
   in ( [ elementVariable ++ " =  document.createElement(\"" ++ nodeName ++ "\");",
          appendChild parent predecessor elementVariable
        ]
          ++ indent childrenContent
          ++ successorContent,
        elementVariable,
        UpdateCallbacks (childrenUpdateCallbacks ++ successorUpdateCallbacks)
      )
compileView (Node exprId (Condition (Expr expr) positiveChildren negativeChildren) : ns) context@(Context (scope, variableStack)) parent predecessor =
  let conditionVariable = scope ++ ".condition" ++ show exprId
      successor = "(" ++ conditionVariable ++ " ? " ++ positiveSuccessor ++ " : " ++ negativeSuccessor ++ ")"
      (positiveChildrenContent, positiveSuccessor, UpdateCallbacks positiveChildrenUpdateCallbacks) = compileView positiveChildren context parent predecessor
      (negativeChildrenContent, negativeSuccessor, UpdateCallbacks negativeChildrenUpdateCallbacks) = compileView negativeChildren context parent predecessor
      internalVariableName = unsafeVariable (publicVariableToInternal variableStack expr)
      (successorContent, successorElement, UpdateCallbacks successorUpdateCallbacks) = compileView ns context parent (Predecessor successor)
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
        UpdateCallbacks ([(internalVariableName, "")] ++ positiveChildrenUpdateCallbacks ++ successorUpdateCallbacks)
      )

-- TODO: a compileerror should be thrown instead
unsafeVariable :: Maybe String -> String
unsafeVariable (Just variable) = variable

type Child = String

appendChild :: Parent -> Predecessor -> Child -> String
appendChild _ (Predecessor predecessor) child = predecessor ++ ".after(" ++ child ++ ");"
appendChild parent FirstElement child = parent ++ ".prepend(" ++ child ++ ");"