module Compiler.Types.View (compileView) where

import Compiler.Types
import Compiler.Util (indent, publicVariableToInternal)
import Types

type Successor = String

type Parent = String

compileView :: [Node View] -> Context -> Parent -> Predecessor -> ([Indent], Successor, UpdateCallbacks, RemoveCallbacks)
compileView [] context _ _ = ([], "null", UpdateCallbacks [], RemoveCallbacks [])
compileView ((Node exprId (StaticText textValue) : ns)) context@(Context (scope, _)) parent predecessor =
  let elementVariable = scope ++ ".el" ++ show exprId
      removeCallback = scope ++ ".removeCallback" ++ show exprId
      (successorContent, successorElement, updateCallbacks, RemoveCallbacks successorRemoveCallback) = compileView ns context parent (Predecessor elementVariable)
   in ( [ Ln (elementVariable ++ " =  document.createTextNode(\"" ++ textValue ++ "\");"),
          Ln (appendChild parent predecessor elementVariable),
          Ln (removeCallback ++ " = () =>" ++ elementVariable ++ ".remove();")
        ]
          ++ successorContent,
        elementVariable,
        updateCallbacks,
        RemoveCallbacks (Ln (removeCallback ++ "();") : successorRemoveCallback)
      )
compileView (Node exprId (DynamicText variable) : ns) context@(Context (scope, variableStack)) parent predecessor =
  let elementVariable = scope ++ "._el" ++ show exprId
      updateCallback = scope ++ ".updateCallback" ++ show exprId
      removeCallback = scope ++ ".removeCallback" ++ show exprId
      (successorContent, successorElement, UpdateCallbacks successorUpdateCallbacks, RemoveCallbacks successorRemoveCallbacks) = compileView ns context parent (Predecessor elementVariable)
      internalVariableName = unsafeVariable (publicVariableToInternal variableStack variable)
   in ( [ Ln (elementVariable ++ " =  document.createTextNode(" ++ internalVariableName ++ ");"),
          Ln (appendChild parent predecessor elementVariable),
          Ln (updateCallback ++ " = () => " ++ elementVariable ++ ".textContent = " ++ internalVariableName ++ ";"),
          Ln (removeCallback ++ " = () => " ++ elementVariable ++ ".remove()")
        ]
          ++ successorContent,
        elementVariable,
        UpdateCallbacks ((internalVariableName, [Ln (updateCallback ++ "();")]) : successorUpdateCallbacks),
        RemoveCallbacks (Ln (removeCallback ++ "();") : successorRemoveCallbacks)
      )
compileView (Node exprId (Host nodeName children option) : ns) context@(Context (scope, _)) parent predecessor =
  let elementVariable = scope ++ ".el" ++ show exprId
      removeCallback = scope ++ ".removeCallback" ++ show exprId
      (childrenContent, _, UpdateCallbacks childrenUpdateCallbacks, _) = compileView children context elementVariable FirstElement
      (successorContent, successorElement, UpdateCallbacks successorUpdateCallbacks, RemoveCallbacks successorRemoveCallbacks) = compileView ns context parent (Predecessor elementVariable)
   in ( [ Ln (elementVariable ++ " =  document.createElement(\"" ++ nodeName ++ "\");"),
          Ln (appendChild parent predecessor elementVariable),
          Ln (removeCallback ++ " = () => " ++ elementVariable ++ ".remove();")
        ]
          ++ childrenContent
          ++ successorContent,
        elementVariable,
        UpdateCallbacks (childrenUpdateCallbacks ++ successorUpdateCallbacks),
        RemoveCallbacks (Ln (removeCallback ++ "();") : successorRemoveCallbacks)
      )
compileView (Node exprId (Condition (Expr expr) positiveChildren negativeChildren) : ns) context@(Context (scope, variableStack)) parent predecessor =
  let conditionVariable = scope ++ ".condition" ++ show exprId
      successor = "(" ++ conditionVariable ++ " ? " ++ positiveSuccessor ++ " : " ++ negativeSuccessor ++ ")"
      (positiveChildrenContent, positiveSuccessor, UpdateCallbacks positiveChildrenUpdateCallbacks, RemoveCallbacks positiveRemoveCallbacks) = compileView positiveChildren context parent predecessor
      (negativeChildrenContent, negativeSuccessor, UpdateCallbacks negativeChildrenUpdateCallbacks, RemoveCallbacks negativeRemoveCallbacks) = compileView negativeChildren context parent predecessor
      internalVariableName = unsafeVariable (publicVariableToInternal variableStack expr)
      createPositiveCallback = scope ++ ".createPositive" ++ show exprId
      createNegativeCallback = scope ++ ".createNegative" ++ show exprId
      removeCallback = scope ++ ".removeCallback" ++ show exprId
      createCallback = "createCondition" ++ show exprId
      updateCallback = scope ++ "updateCondition" ++ show exprId
      (successorContent, successorElement, UpdateCallbacks successorUpdateCallbacks, RemoveCallbacks successorRemoveCallbacks) = compileView ns context parent (Predecessor successor)
   in ( [ Ln (createPositiveCallback ++ " = () => {"),
          Ind positiveChildrenContent,
          Ln "}",
          Ln (createNegativeCallback ++ " = ()=> {"),
          Ind negativeChildrenContent,
          Ln "}",
          Ln ("const " ++ createCallback ++ " = () => {"),
          Ind
            [ Ln (conditionVariable ++ " = " ++ internalVariableName ++ ";"),
              Ln ("if (" ++ conditionVariable ++ ") {"),
              Ind
                [ Ln (createPositiveCallback ++ "();")
                ],
              Ln "} else {",
              Ind
                [ Ln (createNegativeCallback ++ "();")
                ],
              Ln "}"
            ],
          Ln "};",
          Ln (createCallback ++ "();"),
          Ln (updateCallback ++ " = () => {"),
          Ind
            [ Ln (removeCallback ++ "()"),
              Ln (conditionVariable ++ " = " ++ internalVariableName ++ ";"),
              Ln (createCallback ++ "()")
            ],
          Ln "};",
          Ln (removeCallback ++ " = () => {"),
          Ind
            [ Ln ("if( " ++ conditionVariable ++ " ) {"),
              Ind positiveRemoveCallbacks,
              Ln "} else {",
              Ind negativeRemoveCallbacks,
              Ln "}"
            ],
          Ln "}"
        ]
          ++ successorContent,
        successor,
        UpdateCallbacks
          ( [ (internalVariableName, [Ln (updateCallback ++ "();")])
            ]
              ++ [ ( internalVariableName,
                     [ Ln ("if (" ++ conditionVariable ++ ") {"),
                       Ind updateCallback,
                       Ln "}"
                     ]
                   )
                   | (internalVariableName, updateCallback) <- positiveChildrenUpdateCallbacks
                 ]
               ++ [ ( internalVariableName,
                     [ Ln ("if (!" ++ conditionVariable ++ ") {"),
                       Ind updateCallback,
                       Ln "}"
                     ]
                   )
                   | (internalVariableName, updateCallback) <- negativeChildrenUpdateCallbacks
                 ]
              ++ successorUpdateCallbacks
          ),
        RemoveCallbacks successorRemoveCallbacks -- TODO add self removage
      )

-- TODO: a compileerror should be thrown instead
unsafeVariable :: Maybe String -> String
unsafeVariable (Just variable) = variable

type Child = String

appendChild :: Parent -> Predecessor -> Child -> String
appendChild _ (Predecessor predecessor) child = predecessor ++ ".after(" ++ child ++ ");"
appendChild parent FirstElement child = parent ++ ".prepend(" ++ child ++ ");"