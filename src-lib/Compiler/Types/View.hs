module Compiler.Types.View (compileView) where

import Compiler.Types
import Compiler.Util (indent, publicVariableToInternal)
import Types

type Successor = String

type Parent = String

compileView :: [Node View] -> Context -> Parent -> [Predecessor] -> ([Indent], [Predecessor], UpdateCallbacks, RemoveCallbacks)
compileView [] context _ predecessors = ([], predecessors, UpdateCallbacks [], RemoveCallbacks [])
compileView ((Node exprId (StaticText textValue) : ns)) context@(Context (scope, _)) parent predecessors =
  let elementVariable = scope ++ ".el" ++ show exprId
      removeCallback = scope ++ ".removeCallback" ++ show exprId
      (successorContent, predecessors', updateCallbacks, RemoveCallbacks successorRemoveCallback) = compileView ns context parent (Predecessor elementVariable:predecessors)
   in ( [ Ln (elementVariable ++ " =  document.createTextNode(\"" ++ textValue ++ "\");"),
          Ln (appendChild parent predecessors elementVariable),
          Ln (removeCallback ++ " = () =>" ++ elementVariable ++ ".remove();")
        ]
          ++ successorContent,
        predecessors',
        updateCallbacks,
        RemoveCallbacks (Ln (removeCallback ++ "();") : successorRemoveCallback)
      )
compileView (Node exprId (DynamicText variable) : ns) context@(Context (scope, variableStack)) parent predecessors =
  let elementVariable = scope ++ "._el" ++ show exprId
      updateCallback = scope ++ ".updateCallback" ++ show exprId
      removeCallback = scope ++ ".removeCallback" ++ show exprId
      (successorContent, predecessors', UpdateCallbacks successorUpdateCallbacks, RemoveCallbacks successorRemoveCallbacks) = compileView ns context parent (Predecessor elementVariable:predecessors)
      internalVariableName = unsafeVariable (publicVariableToInternal variableStack variable)
   in ( [ Ln (elementVariable ++ " =  document.createTextNode(" ++ internalVariableName ++ ");"),
          Ln (appendChild parent predecessors elementVariable),
          Ln (updateCallback ++ " = () => " ++ elementVariable ++ ".textContent = " ++ internalVariableName ++ ";"),
          Ln (removeCallback ++ " = () => " ++ elementVariable ++ ".remove()")
        ]
          ++ successorContent,
        predecessors',
        UpdateCallbacks ((internalVariableName, [Ln (updateCallback ++ "();")]) : successorUpdateCallbacks),
        RemoveCallbacks (Ln (removeCallback ++ "();") : successorRemoveCallbacks)
      )
compileView (Node exprId (Host nodeName children option) : ns) context@(Context (scope, _)) parent predecessors =
  let elementVariable = scope ++ ".el" ++ show exprId
      removeCallback = scope ++ ".removeCallback" ++ show exprId
      (childrenContent, _, UpdateCallbacks childrenUpdateCallbacks, _) = compileView children context elementVariable []
      (successorContent, predecessors', UpdateCallbacks successorUpdateCallbacks, RemoveCallbacks successorRemoveCallbacks) = compileView ns context parent (Predecessor elementVariable:predecessors)
   in ( [ Ln (elementVariable ++ " =  document.createElement(\"" ++ nodeName ++ "\");"),
          Ln (appendChild parent predecessors elementVariable),
          Ln (removeCallback ++ " = () => " ++ elementVariable ++ ".remove();")
        ]
          ++ childrenContent
          ++ successorContent,
        predecessors',
        UpdateCallbacks (childrenUpdateCallbacks ++ successorUpdateCallbacks),
        RemoveCallbacks (Ln (removeCallback ++ "();") : successorRemoveCallbacks)
      )
compileView (Node exprId (Each [Attribute (LeftTuple [LeftVariable publicEntityVariable, LeftVariable publicIndexVariable], FeedOperator, Expr variable)] entityChildren negativeChildren) : ns) context@(Context (scope, variableStack)) parent predecessors =
  let indexVariable = "index" ++ show exprId
      entitiesScope = scope ++ ".entities" ++ show exprId
      entityScope = entitiesScope ++ "[" ++ indexVariable ++ "]"
      entityValue = entityScope ++ ".value"
      internalEntitiesVariable = unsafeVariable (publicVariableToInternal variableStack variable)
      createEntityCallback = "createEach" ++ show exprId
      counter = scope ++ "." ++ indexVariable
      predecessorOf = scope ++ ".getPredecessorOf" ++ show exprId
      successor = predecessorOf ++ "(" ++ counter ++ " - 1)"
      entityPredecessor = predecessorOf ++ "(" ++ indexVariable ++ " - 1)"
      entityVariable = "entity" ++ show exprId
      entityVariableStack = (publicIndexVariable, indexVariable) : (publicEntityVariable, entityValue) : variableStack
      (entityChildrenContent, entitySuccessor, UpdateCallbacks entityChildrenUpdateCallbacks, RemoveCallbacks positiveRemoveCallbacks) = compileView entityChildren (Context (entityScope, entityVariableStack)) parent (Predecessor entityPredecessor:predecessors)
      (negativeChildrenContent, negativeSuccessor, UpdateCallbacks negativeChildrenUpdateCallbacks, RemoveCallbacks negativeRemoveCallbacks) = compileView negativeChildren context parent predecessors
      (successorContent, predecessors', UpdateCallbacks successorUpdateCallbacks, RemoveCallbacks successorRemoveCallbacks) = compileView ns context parent [Predecessor successor]
   in ( [ Ln (entitiesScope ++ " = [];"),
          Ln (predecessorOf ++ " = (" ++ indexVariable ++ ") => {"),
          Ind
            [ Ln ("if (" ++ indexVariable ++ " < 0) {"),
              Ind [
                Ln ("return " ++ predecessorChain predecessors)
              ],
              Ln ("} else if (" ++ counter ++ " === 0) {"),
              Ind
                [ Ln ("return " ++ predecessorChain negativeSuccessor)
                ],
              Ln "} else {",
              Ind
                [ Ln ("return " ++ predecessorChain entitySuccessor)
                ],
              Ln "}"
            ],
          Ln "}",
          Ln ("const " ++ createEntityCallback ++ " = (" ++ indexVariable ++ ") => {"),
          Ind entityChildrenContent,
          Ln "}",
          Ln "",
          Ln (counter ++ " = 0;"),
          Ln ("for (const " ++ entityVariable ++ " of " ++ internalEntitiesVariable ++ ") {"),
          Ind
            [ Ln (entitiesScope ++ ".push({value : " ++ entityVariable ++ "})"),
              Ln (createEntityCallback ++ "(" ++ counter ++ ");"),
              Ln (counter ++ "++;")
            ],
          Ln "}"
        ]
          ++ successorContent,
        predecessors',
        UpdateCallbacks
          [ (internalEntitiesVariable, [])
          ],
        RemoveCallbacks []
      )
compileView (Node exprId (Condition (Expr expr) positiveChildren negativeChildren) : ns) context@(Context (scope, variableStack)) parent predecessors =
  let conditionVariable = scope ++ ".condition" ++ show exprId
      (positiveChildrenContent, positiveSuccessor, UpdateCallbacks positiveChildrenUpdateCallbacks, RemoveCallbacks positiveRemoveCallbacks) = compileView positiveChildren context parent predecessors
      (negativeChildrenContent, negativeSuccessor, UpdateCallbacks negativeChildrenUpdateCallbacks, RemoveCallbacks negativeRemoveCallbacks) = compileView negativeChildren context parent predecessors
      successor = "(" ++ conditionVariable ++ " ? " ++ predecessorChain positiveSuccessor ++ " : " ++ predecessorChain negativeSuccessor ++ ")"
      internalVariableName = unsafeVariable (publicVariableToInternal variableStack expr)
      createPositiveCallback = scope ++ ".createPositive" ++ show exprId
      createNegativeCallback = scope ++ ".createNegative" ++ show exprId
      removeCallback = scope ++ ".removeCallback" ++ show exprId
      createCallback = "createCondition" ++ show exprId
      updateCallback = scope ++ "updateCondition" ++ show exprId
      (successorContent, predecessors', UpdateCallbacks successorUpdateCallbacks, RemoveCallbacks successorRemoveCallbacks) = compileView ns context parent (Predecessor successor:predecessors)
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
        predecessors',
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

appendChild :: Parent -> [Predecessor] -> Child -> String
appendChild parent [] child = parent ++ ".prepend(" ++ child ++ ");"
appendChild _ ((Predecessor predecessor):ps) child = predecessor ++ ".after(" ++ child ++ ");"

predecessorChain :: [Predecessor] -> String
predecessorChain (Predecessor predecessor:ps) = predecessor
predecessorChain (MaybePredecessor predecessor:ps) = predecessor ++ " || " ++ predecessorChain ps
predecessorChain [] = "null"