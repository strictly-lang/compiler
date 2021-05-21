module Compiler.Types.View (compileView) where

import Compiler.Types
import Compiler.Util (filter', functionDefinitionToJs, indent, publicVariableToInternal, rightHandSideValueFunctionCallToJs, rightHandSideValueToJs)
import Data.List (intercalate, isPrefixOf)
import Types

type Successor = String

type ExprId = Int

type Parent = String

firstOfTriplet :: (a, b, c) -> a
firstOfTriplet (a, _, _) = a

secondOfTriplet :: (a, b, c) -> b
secondOfTriplet (_, b, _) = b

lastOfTriplet :: (a, b, c) -> c
lastOfTriplet (_, _, c) = c

compileView :: [ViewContent] -> ExprId -> Context -> Parent -> [Predecessor] -> ([Indent], ExprId, [Predecessor], UpdateCallbacks, RemoveCallbacks)
compileView [] exprId context _ predecessors = ([], exprId, predecessors, UpdateCallbacks [], RemoveCallbacks [])
compileView (((MixedText texts) : ns)) exprId context@(Context (scope, variableStack)) parent predecessors =
  let elementVariableFactory = \exprId' -> scope ++ ".el" ++ show exprId'
      predecessorFactory = \exprId' -> if exprId' == exprId then predecessors else [Predecessor (elementVariableFactory (exprId' - 1))]
      textContents =
        [ case text of
            DynamicText rightHandValue ->
              let elementVariable = elementVariableFactory exprId'
                  updateCallback = scope ++ ".updateCallback" ++ show exprId'
                  removeCallback = scope ++ ".removeCallback" ++ show exprId'
                  rightHandJsValue = rightHandSideValueToJs variableStack rightHandValue
               in ( Ln (elementVariable ++ " =  document.createTextNode(") :
                    fst rightHandJsValue
                      ++ [ Ln ");",
                           Br,
                           Ln (appendChild parent (predecessorFactory exprId') elementVariable),
                           Br,
                           Ln (removeCallback ++ " = () => " ++ elementVariable ++ ".remove()"),
                           Br
                         ],
                    [ ( dependency,
                        [Ln (elementVariable ++ ".textContent = ")] ++ fst rightHandJsValue ++ [Ln ";", Br]
                      )
                      | dependency <- snd rightHandJsValue
                    ],
                    [Ln (removeCallback ++ "();"), Br]
                  )
            StaticText content ->
              let elementVariable = elementVariableFactory exprId'
                  removeCallback = scope ++ ".removeCallback" ++ show exprId'
               in ( [ Ln (elementVariable ++ " =  document.createTextNode(\"" ++ content ++ "\");"),
                      Br,
                      Ln (appendChild parent (predecessorFactory exprId') elementVariable),
                      Br,
                      Ln (removeCallback ++ " = () => " ++ elementVariable ++ ".remove();"),
                      Br
                    ],
                    [],
                    [Ln (removeCallback ++ "();"), Br]
                  )
          | (text, exprId') <- zip texts [exprId ..]
        ]
      successorExprId = exprId + length texts
      gfhg = concatMap secondOfTriplet textContents
      (successorContent, exprId', predecessors', UpdateCallbacks successorUpdateCallbacks, RemoveCallbacks successorRemoveCallback) = compileView ns successorExprId context parent (predecessorFactory successorExprId ++ predecessors)
   in ( concatMap firstOfTriplet textContents ++ successorContent,
        exprId',
        predecessors',
        UpdateCallbacks (concatMap secondOfTriplet textContents ++ successorUpdateCallbacks),
        RemoveCallbacks (concatMap lastOfTriplet textContents ++ successorRemoveCallback)
      )
compileView ((Host nodeName options children) : ns) exprId context@(Context (scope, variableStack)) parent predecessors =
  let elementVariable = scope ++ ".el" ++ show exprId
      removeCallback = scope ++ ".removeCallback" ++ show exprId
      (childrenContent, exprId', _, UpdateCallbacks childrenUpdateCallbacks, _) = compileView children (exprId + 1) context elementVariable []
      (successorContent, exprId'', predecessors', UpdateCallbacks successorUpdateCallbacks, RemoveCallbacks successorRemoveCallbacks) = compileView ns (exprId' + 1) context parent (Predecessor elementVariable : predecessors)
      getAttributeValue = \attributeRightHandSide -> ([rightHandSideValueToJs variableStack singleAttributeRightHandSide | RightHandSideValue singleAttributeRightHandSide <- attributeRightHandSide])
   in ( [Ln (elementVariable ++ " =  document.createElement(\"" ++ nodeName ++ "\");"), Br]
          ++ concat
            [ if "on" `isPrefixOf` attributeKey
                then [Ln (elementVariable ++ "." ++ attributeKey ++ " = ")] ++ functionDefinitionToJs variableStack attributeRightHandSide ++ [Br]
                else Ln (elementVariable ++ ".setAttribute(\"" ++ attributeKey ++ "\", ") : concatMap fst (getAttributeValue attributeRightHandSide) ++ [Ln ");", Br]
              | (attributeKey, attributeRightHandSide) <- options
            ]
          ++ [ Ln (appendChild parent predecessors elementVariable),
               Br,
               Ln (removeCallback ++ " = () => " ++ elementVariable ++ ".remove();"),
               Br
             ]
          ++ childrenContent
          ++ successorContent,
        exprId'',
        predecessors',
        UpdateCallbacks
          ( [ ( dependency,
                Ln (elementVariable ++ ".setAttribute(\"" ++ attributeKey ++ "\", ") : attributeJs ++ [Ln ");", Br]
              )
              | (attributeKey, attributeRightHandSide) <- options,
                (attributeJs, dependencies) <- getAttributeValue attributeRightHandSide,
                dependency <- dependencies
            ]
              ++ childrenUpdateCallbacks
              ++ successorUpdateCallbacks
          ),
        RemoveCallbacks (Ln (removeCallback ++ "();") : successorRemoveCallbacks)
      )
compileView ((ViewModel (Expression (LeftTuple [LeftVariable publicStateVariable, LeftVariable publicDispatchVariable], FeedOperator, sourceValue)) children) : ns) exprId context@(Context (scope, variableStack)) parent predecessors =
  let modelScope = scope ++ ".model" ++ show exprId
      modelState = modelScope ++ "[0]"
      dispatcher = modelScope ++ "[1]"
      modeledVariableStack = ([publicStateVariable], modelState) : ([publicDispatchVariable], dispatcher) : variableStack
      (childrenContent, exprId', predecessors', UpdateCallbacks childrenUpdateCallbacks, removeCallbacks) = compileView children (exprId + 1) (Context (scope, modeledVariableStack)) parent predecessors
      (modelUpdateCallback, restUpdateCallbacks) = filter' ((== modelState) . fst) childrenUpdateCallbacks
      (modelValue, modelDependencies) = rightHandSideValueFunctionCallToJs (map snd modelUpdateCallback) variableStack sourceValue
      (successorContent, exprId''', predecessors'', UpdateCallbacks successorUpdateCallbacks, RemoveCallbacks successorRemoveCallbacks) = compileView ns (exprId' + 1) context parent predecessors'
   in ( [Ln (modelScope ++ " = ")] ++ modelValue ++ [Br]
          ++ childrenContent
          ++ successorContent,
        exprId',
        predecessors'',
        UpdateCallbacks restUpdateCallbacks,
        removeCallbacks
      )
compileView ((Each [Expression (LeftTuple [LeftVariable publicEntityVariable, LeftVariable publicIndexVariable], FeedOperator, sourceValue)] entityChildren negativeChildren) : ns) exprId context@(Context (scope, variableStack)) parent predecessors =
  let indexVariable = "index" ++ show exprId
      entitiesScope = scope ++ ".entities" ++ show exprId
      entityScope = entitiesScope ++ "[" ++ indexVariable ++ "]"
      entityValue = entityScope ++ ".value"
      updateCallback = scope ++ ".updateCallback" ++ show exprId
      (internalEntitiesVariable, sourceValueDependencies) = rightHandSideValueToJs variableStack sourceValue
      createEntityCallback = "createEach" ++ show exprId
      predecessorOf = scope ++ ".getPredecessorOf" ++ show exprId
      successor = predecessorOf ++ "(" ++ entitiesScope ++ ".length - 1)"
      entityPredecessor = predecessorOf ++ "(" ++ indexVariable ++ " - 1)"
      entityVariable = "entity" ++ show exprId
      entityVariableStack = ([publicIndexVariable], indexVariable) : ([publicEntityVariable], entityValue) : variableStack
      (entityChildrenContent, exprId', entitySuccessor, UpdateCallbacks entityChildrenUpdateCallbacks, RemoveCallbacks positiveRemoveCallbacks) = compileView entityChildren (exprId + 1) (Context (entityScope, entityVariableStack)) parent (Predecessor entityPredecessor : predecessors)
      (negativeChildrenContent, exprId'', negativeSuccessor, UpdateCallbacks negativeChildrenUpdateCallbacks, RemoveCallbacks negativeRemoveCallbacks) = compileView negativeChildren (exprId' + 1) context parent predecessors
      (successorContent, exprId''', predecessors', UpdateCallbacks successorUpdateCallbacks, RemoveCallbacks successorRemoveCallbacks) = compileView ns (exprId'' + 1) context parent [Predecessor successor]
      (_, updateCallbacks) = filter' ((== indexVariable) . fst) entityChildrenUpdateCallbacks
      (entityUpdateCallback, restUpdateCallbacks') = filter' ((== entityValue) . fst) updateCallbacks
   in ( [ Ln (entitiesScope ++ " = [];"),
          Br,
          Ln (predecessorOf ++ " = (" ++ indexVariable ++ ") => {"),
          Br,
          Ind
            [ Ln ("if (" ++ indexVariable ++ " < 0) {"),
              Br,
              Ind
                [ Ln ("return " ++ predecessorChain predecessors),
                  Br
                ],
              Ln ("} else if (" ++ entityScope ++ ".length === 0) {"),
              Br,
              Ind
                [ Ln ("return " ++ predecessorChain negativeSuccessor),
                  Br
                ],
              Br,
              Ln "} else {",
              Br,
              Ind
                [ Ln ("return " ++ predecessorChain entitySuccessor),
                  Br
                ],
              Ln "}",
              Br
            ],
          Ln "}",
          Br,
          Ln ("const " ++ createEntityCallback ++ " = (" ++ indexVariable ++ ") => {"),
          Br,
          Ind entityChildrenContent,
          Br,
          Ln "}",
          Br,
          Ln ("let " ++ indexVariable ++ " = 0;"),
          Br,
          Ln ("for (const " ++ entityVariable ++ " of ")
        ]
          ++ internalEntitiesVariable
          ++ [ Ln ") {",
               Br,
               Ind
                 [ Ln (entitiesScope ++ ".push({value : " ++ entityVariable ++ "});"),
                   Br,
                   Ln (createEntityCallback ++ "(" ++ indexVariable ++ ");"),
                   Br,
                   Ln (indexVariable ++ "++;"),
                   Br
                 ],
               Ln "}",
               Br,
               Ln (updateCallback ++ "= () => {"),
               Br,
               Ind
                 ( [ Ln ("let " ++ indexVariable ++ " = 0;"),
                     Br,
                     Ln ("for (const " ++ entityVariable ++ " of ")
                   ]
                     ++ internalEntitiesVariable
                     ++ [ Ln ") {",
                          Br,
                          Ind
                            [ Ln ("if (" ++ indexVariable ++ " < " ++ entitiesScope ++ ".length) {"),
                              Br,
                              Ind
                                ( Ln (entityValue ++ " = " ++ entityVariable ++ ";") : Br : 
                                  concatMap
                                    ( \singleEntityUpdateCallback ->
                                        snd singleEntityUpdateCallback ++ [Br]
                                    )
                                    entityUpdateCallback
                                ),
                              Br,
                              Ln "} else {",
                              Br,
                              Ind
                                [ Ln (entitiesScope ++ ".push({value : " ++ entityVariable ++ "});"),
                                  Br,
                                  Ln (createEntityCallback ++ "(" ++ indexVariable ++ ")"),
                                  Br
                                ],
                              Ln "}",
                              Br,
                              Ln (indexVariable ++ "++;"),
                              Br
                            ],
                          Ln "}",
                          Br,
                          Ln ("let newCount = " ++ indexVariable ++ ";"),
                          Br,
                          Ln ("for (let " ++ indexVariable ++ " = " ++ entitiesScope ++ ".length - 1; " ++ indexVariable ++ " >= newCount; " ++ indexVariable ++ "--) {"),
                          Br,
                          Ind
                            ( concatMap (\positiveRemoveCallback -> positiveRemoveCallback : [Br]) positiveRemoveCallbacks
                                ++ [Ln (entitiesScope ++ ".pop();"), Br]
                            ),
                          Ln "}",
                          Br
                        ]
                 ),
               Ln "}",
               Br
             ]
          ++ successorContent,
        exprId''',
        predecessors',
        UpdateCallbacks
          ( [(dependency, [Ln (updateCallback ++ "();"), Br]) | dependency <- sourceValueDependencies] ++ restUpdateCallbacks'
          ),
        RemoveCallbacks []
      )
compileView ((Condition conditionValue positiveChildren negativeChildren) : ns) exprId context@(Context (scope, variableStack)) parent predecessors =
  let conditionVariable = scope ++ ".condition" ++ show exprId
      (positiveChildrenContent, exprId', positiveSuccessor, UpdateCallbacks positiveChildrenUpdateCallbacks, RemoveCallbacks positiveRemoveCallbacks) = compileView positiveChildren (exprId + 1) context parent predecessors
      (negativeChildrenContent, exprId'', negativeSuccessor, UpdateCallbacks negativeChildrenUpdateCallbacks, RemoveCallbacks negativeRemoveCallbacks) = compileView negativeChildren (exprId' + 1) context parent predecessors
      successor = "(" ++ conditionVariable ++ " ? " ++ predecessorChain positiveSuccessor ++ " : " ++ predecessorChain negativeSuccessor ++ ")"
      (internalConditionValue, conditionValueDependencies) = rightHandSideValueToJs variableStack conditionValue
      createPositiveCallback = scope ++ ".createPositive" ++ show exprId
      createNegativeCallback = scope ++ ".createNegative" ++ show exprId
      removeCallback = scope ++ ".removeCallback" ++ show exprId
      createCallback = "createCondition" ++ show exprId
      updateCallback = scope ++ ".updateCondition" ++ show exprId
      (successorContent, exprId''', predecessors', UpdateCallbacks successorUpdateCallbacks, RemoveCallbacks successorRemoveCallbacks) = compileView ns (exprId'' + 1) context parent (Predecessor successor : predecessors)
   in ( [ Ln (createPositiveCallback ++ " = () => {"),
          Br,
          Ind positiveChildrenContent,
          Ln "}",
          Br,
          Ln (createNegativeCallback ++ " = ()=> {"),
          Br,
          Ind negativeChildrenContent,
          Ln "}",
          Br,
          Ln ("const " ++ createCallback ++ " = () => {"),
          Br,
          Ind
            ( [Ln (conditionVariable ++ " = ")]
                ++ internalConditionValue
                ++ [ Ln ";",
                     Br,
                     Ln ("if (" ++ conditionVariable ++ ") {"),
                     Br,
                     Ind
                       [ Ln (createPositiveCallback ++ "();")
                       ],
                     Ln "} else {",
                     Ind
                       [ Ln (createNegativeCallback ++ "();")
                       ],
                     Ln "}"
                   ]
            ),
          Ln "};",
          Br,
          Ln (createCallback ++ "();"),
          Br,
          Ln (updateCallback ++ " = () => {"),
          Br,
          Ind
            ( [ Ln (removeCallback ++ "();"),
            Br,
                Ln (conditionVariable ++ " = ")
              ]
                ++ internalConditionValue
                ++ [ Ln ";",
                     Br,
                     Ln (createCallback ++ "();"),
                     Br
                   ]
            ),
          Ln "};",
          Br,
          Ln (removeCallback ++ " = () => {"),
          Br,
          Ind
            [ Ln ("if( " ++ conditionVariable ++ " ) {"),
              Br,
              Ind positiveRemoveCallbacks,
              Br,
              Ln "} else {",
              Br,
              Ind negativeRemoveCallbacks,
              Ln "}",
              Br
            ],
          Ln "}",
          Br
        ]
          ++ successorContent,
        exprId''',
        predecessors',
        UpdateCallbacks
          ( -- [ (internalVariableName, [Ln (updateCallback ++ "();")])
            -- ]
            [(dependency, [Ln (updateCallback ++ "()")]) | dependency <- conditionValueDependencies]
              -- TODO move to inline code section, instead of in callback section
              ++ [ ( internalVariableName,
                     [ Ln ("if (" ++ conditionVariable ++ ") {"),
                       Br,
                       Ind updateCallback,
                       Ln "}",
                       Br
                     ]
                   )
                   | (internalVariableName, updateCallback) <- positiveChildrenUpdateCallbacks
                 ]
              ++ [ ( internalVariableName,
                     [ Ln ("if (!" ++ conditionVariable ++ ") {"),
                       Br,
                       Ind updateCallback,
                       Ln "}",
                       Br
                     ]
                   )
                   | (internalVariableName, updateCallback) <- negativeChildrenUpdateCallbacks
                 ]
              ++ successorUpdateCallbacks
          ),
        RemoveCallbacks successorRemoveCallbacks -- TODO add self removage
      )

type Child = String

appendChild :: Parent -> [Predecessor] -> Child -> String
appendChild parent [] child = parent ++ ".prepend(" ++ child ++ ");"
appendChild _ ((Predecessor predecessor) : ps) child = predecessor ++ ".after(" ++ child ++ ");"

predecessorChain :: [Predecessor] -> String
predecessorChain (Predecessor predecessor : ps) = predecessor
predecessorChain (MaybePredecessor predecessor : ps) = predecessor ++ " || " ++ predecessorChain ps
predecessorChain [] = "null"