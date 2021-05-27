module Compiler.Types.View (compileView) where

import Compiler.Types
import Compiler.Util (filter', functionToJs, indent, leftHandSideToJs, publicVariableToInternal, rightHandSideValueFunctionCallToJs, rightHandSideValueToJs)
import Data.List (intercalate, intersperse, isPrefixOf)
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

type CompileResult = ([Indent], ExprId, [Predecessor], UpdateCallbacks, RemoveCallbacks)

compileView :: [ViewContent] -> ExprId -> Context -> Parent -> [Predecessor] -> CompileResult
compileView [] exprId context _ predecessors = ([], exprId, predecessors, UpdateCallbacks [], RemoveCallbacks [])
compileView (((MixedText texts) : ns)) exprId context@(Context (scope, variableStack)) parent predecessors =
  let elementVariableFactory = \exprId' -> scope ++ ".el" ++ show exprId'
      predecessorFactory = \exprId' -> if exprId' == exprId then predecessors else [Predecessor (elementVariableFactory (exprId' - 1))]
      textContents =
        [ case text of
            DynamicText rightHandValue ->
              let elementVariable = elementVariableFactory exprId'
                  updateCallback = scope ++ ".updateCallback" ++ show exprId'
                  rightHandJsValue = rightHandSideValueToJs variableStack rightHandValue
               in ( Ln (elementVariable ++ " = document.createTextNode(") :
                    fst rightHandJsValue
                      ++ [ Ln ");",
                           Br,
                           Ln (appendChild parent (predecessorFactory exprId') elementVariable),
                           Br
                         ],
                    [ ( dependency,
                        [Ln (elementVariable ++ ".textContent = ")] ++ fst rightHandJsValue ++ [Ln ";", Br]
                      )
                      | dependency <- snd rightHandJsValue
                    ],
                    [Ln (elementVariable ++ ".remove();"), Br]
                  )
            StaticText content ->
              let elementVariable = elementVariableFactory exprId'
               in ( [ Ln (elementVariable ++ " = document.createTextNode(\"" ++ content ++ "\");"),
                      Br,
                      Ln (appendChild parent (predecessorFactory exprId') elementVariable),
                      Br
                    ],
                    [],
                    [Ln (elementVariable ++ ".remove();"), Br]
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
      (childrenContent, exprId', _, UpdateCallbacks childrenUpdateCallbacks, _) = compileView children (exprId + 1) context elementVariable []
      (successorContent, exprId'', predecessors', UpdateCallbacks successorUpdateCallbacks, RemoveCallbacks successorRemoveCallbacks) = compileView ns (exprId' + 1) context parent (Predecessor elementVariable : predecessors)
      getAttributeValue = \attributeRightHandSide -> ([rightHandSideValueToJs variableStack singleAttributeRightHandSide | RightHandSideValue singleAttributeRightHandSide <- attributeRightHandSide])
   in ( [Ln (elementVariable ++ " = document.createElement(\"" ++ nodeName ++ "\");"), Br]
          ++ concat
            [ if "on" `isPrefixOf` attributeKey
                then [Ln (elementVariable ++ ".addEventListener(\"" ++ drop 2 attributeKey ++ "\", ")] ++ functionToJs variableStack attributeRightHandSide ++ [Ln ");", Br]
                else Ln (elementVariable ++ ".setAttribute(\"" ++ attributeKey ++ "\", ") : concatMap fst (getAttributeValue attributeRightHandSide) ++ [Ln ");", Br]
              | (attributeKey, attributeRightHandSide) <- options
            ]
          ++ [ Ln (appendChild parent predecessors elementVariable),
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
        RemoveCallbacks (Ln (elementVariable ++ ".remove();") : successorRemoveCallbacks)
      )
compileView ((ViewModel (Expression (LeftTuple [LeftVariable publicStateVariable, LeftVariable publicDispatchVariable], FeedOperator, sourceValue)) children) : ns) exprId context@(Context (scope, variableStack)) parent predecessors =
  let modelScope = scope ++ ".model" ++ show exprId
      modelState = modelScope ++ "[0]"
      dispatcher = modelScope ++ "[1]"
      modeledVariableStack = ([publicStateVariable], modelState) : ([publicDispatchVariable], dispatcher) : variableStack
      (childrenContent, exprId', predecessors', UpdateCallbacks childrenUpdateCallbacks, removeCallbacks) = compileView children (exprId + 1) (Context (scope, modeledVariableStack)) parent predecessors
      (modelUpdateCallback, restUpdateCallbacks) = filter' ((== modelState) . fst) childrenUpdateCallbacks
      modelUpdateCallbackJs =
        [ Ln "() => {",
          Br,
          Ind (intercalate [Br] (map snd modelUpdateCallback)),
          Br,
          Ln "}"
        ]
      (modelValue, modelDependencies) = rightHandSideValueFunctionCallToJs [modelUpdateCallbackJs] variableStack sourceValue
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
      createEntityCallback = scope ++ ".createEach" ++ show exprId
      createElseCallback = scope ++ ".createElse" ++ show exprId
      predecessorOf = scope ++ ".getPredecessorOf" ++ show exprId
      successor = predecessorOf ++ "(" ++ entitiesScope ++ ".length - 1)"
      entityPredecessor = predecessorOf ++ "(" ++ indexVariable ++ " - 1)"
      entityVariable = "entity" ++ show exprId
      entityVariableStack = ([publicIndexVariable], indexVariable) : ([publicEntityVariable], entityValue) : variableStack
      (entityChildrenContent, exprId', entitySuccessor, UpdateCallbacks entityChildrenUpdateCallbacks, RemoveCallbacks positiveRemoveCallbacks) = compileView entityChildren (exprId + 1) (Context (entityScope, entityVariableStack)) parent (Predecessor entityPredecessor : predecessors)
      (negativeChildrenContent, exprId'', negativeSuccessor, UpdateCallbacks negativeChildrenUpdateCallbacks, RemoveCallbacks negativeRemoveCallbacks) = compileView negativeChildren (exprId' + 1) context parent predecessors
      (successorContent, exprId''', predecessors', UpdateCallbacks successorUpdateCallbacks, RemoveCallbacks successorRemoveCallbacks) = compileView ns (exprId'' + 1) context parent [Predecessor successor]
      (_, updateCallbacks) = filter' ((== indexVariable) . fst) entityChildrenUpdateCallbacks -- index-variable-value of a single entity is unchangable, therefore needs to tracking
      (entityUpdateCallback, restEntityUpdateCallbacks) = filter' ((== entityValue) . fst) updateCallbacks
   in ( [ Ln (entitiesScope ++ " = [];"),
          Br,
          Ln (predecessorOf ++ " = (" ++ indexVariable ++ ") => {"),
          Br,
          Ind
            [ Ln ("if (" ++ entitiesScope ++ ".length === 0) {"),
              Br,
              Ind
                [ Ln ("return " ++ predecessorChain negativeSuccessor),
                  Br
                ],
              Br,
              Ln ("} else if (" ++ indexVariable ++ " < 0) {"),
              Br,
              Ind
                [ Ln ("return " ++ predecessorChain predecessors),
                  Br
                ],
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
          Ln (createEntityCallback ++ " = (" ++ indexVariable ++ ") => {"),
          Br,
          Ind entityChildrenContent,
          Br,
          Ln "}",
          Br,
          Ln (createElseCallback ++ " = () => {"),
          Br,
          Ind negativeChildrenContent,
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
               Ln ("if (" ++ indexVariable ++ " === 0) {"),
               Br,
               Ind
                 [ Ln (createElseCallback ++ "();")
                 ],
               Br,
               Ln "}",
               Br,
               Ln (updateCallback ++ "= () => {"),
               Br,
               Ind
                 ( [ Ln ("let " ++ indexVariable ++ " = 0;"),
                     Br,
                     Ln ("const previousLength = " ++ entitiesScope ++ ".length"),
                     Br,
                     Ln ("for (const " ++ entityVariable ++ " of ")
                   ]
                     ++ internalEntitiesVariable
                     ++ [ Ln ") {",
                          Br,
                          Ind
                            [ Ln ("if (" ++ indexVariable ++ " === 0 && previousLength === 0) {"),
                              Br,
                              Ind negativeRemoveCallbacks,
                              Br,
                              Ln "}",
                              Br,
                              Ln ("if (" ++ indexVariable ++ " < " ++ entitiesScope ++ ".length) {"),
                              Br,
                              Ind
                                ( Ln (entityValue ++ " = " ++ entityVariable ++ ";") :
                                  Br :
                                  concatMap
                                    snd
                                    entityUpdateCallback
                                ),
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
                          Br,
                          Ln "if (newCount === 0 && previousLength !== 0) {",
                          Br,
                          Ind
                            [ Ln (createElseCallback ++ "();")
                            ],
                          Br,
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
          ( [(dependency, [Ln (updateCallback ++ "();"), Br]) | dependency <- sourceValueDependencies]
              ++ [ ( dependency,
                     [Ln ("let " ++ indexVariable ++ " = 0;"), Br, Ln ("for (const " ++ entityVariable ++ " of ")] ++ internalEntitiesVariable
                       ++ [Ln ") {", Br, Ind (restEntityUpdateCallback ++ [Ln (indexVariable ++ "++;"), Br]), Ln "}", Br]
                   )
                   | (dependency, restEntityUpdateCallback) <- restEntityUpdateCallbacks
                 ]
              ++ [ ( dependency,
                     [ Ln ("if( " ++ entitiesScope ++ ".length === 0 ) {"),
                       Br,
                       Ind negativepdateCallback,
                       Ln "}",
                       Br
                     ]
                   )
                   | (dependency, negativepdateCallback) <- negativeChildrenUpdateCallbacks
                 ]
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
                     Br,
                     Ln "} else {",
                     Br,
                     Ind
                       [ Ln (createNegativeCallback ++ "();")
                       ],
                     Br,
                     Ln "}",
                     Br
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
              Br,
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
          ( [(dependency, [Ln (updateCallback ++ "();")]) | dependency <- conditionValueDependencies]
              -- TODO move to inline code section, instead of in callback section
              ++ [ ( internalVariableName,
                     [ Ln ("if (" ++ conditionVariable ++ ") {"),
                       Br,
                       Ind updateCallback,
                       Br,
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
                       Br,
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
compileView ((Match rightHandValue cases : ns)) exprId context@(Context (scope, variableStack)) parent predecessors =
  let currentValueVariable = scope ++ ".currentValue" ++ show exprId
      currentCaseVariable = scope ++ ".currentCase" ++ show exprId
      updateCallback = scope ++ ".updateCallback" ++ show exprId
      (rightHandValueJs, dependencies) = rightHandSideValueToJs variableStack rightHandValue
      (patterns, exprId') = getMatchPatterns cases currentValueVariable (exprId + 1) context parent predecessors
      (successorContent, exprId'', predecessors', UpdateCallbacks successorUpdateCallbacks, RemoveCallbacks successorRemoveCallback) = compileView ns exprId' context parent (predecessors)
   in ( [ Ln (currentValueVariable ++ " = undefined;"),
          Br,
          Ln (currentCaseVariable ++ " = undefined;"),
          Br,
          Ln (updateCallback ++ " = () => {"),
          Br,
          Ind
            ( [ Ln ("const previousValue = " ++ currentValueVariable ++ ";"),
                Br,
                Ln ("const previousCase = " ++ currentCaseVariable ++ ";"),
                Br,
                Ln (currentValueVariable ++ " = ")
              ]
                ++ rightHandValueJs
                ++ [ Br,
                     Ln (currentCaseVariable ++ " = "),
                     Br,
                     Ind (getCaseCondition 0 (map fst patterns)),
                     Br
                   ]
                ++ intercalate
                  [Br]
                  [ [ Ln ("if (" ++ currentCaseVariable ++ " === " ++ show index ++ ") {"),
                      Br,
                      Ind [],
                      Br,
                      Ln "}"
                    ]
                    | ((_, compileResult), index) <- zip patterns [0 ..]
                  ]
            ),
          Br,
          Ln "}",
          Br,
          Ln (updateCallback ++ "();"),
          Br
        ]
          ++ successorContent,
        exprId',
        predecessors',
        UpdateCallbacks (successorUpdateCallbacks),
        RemoveCallbacks (successorRemoveCallback)
      )

type Index = Int

getMatchPatterns :: [Case] -> InternalVariableName -> ExprId -> Context -> Parent -> [Predecessor] -> ([([Indent], CompileResult)], ExprId)
getMatchPatterns [] internalVariableName exprId context parent predecessors = ([], exprId)
getMatchPatterns ((Case leftHandSide children) : cases) internalVariableName exprId context@(Context (scope, variableStack)) parent predecessors =
  let (conditions, variableStack') = leftHandSideToJs variableStack internalVariableName leftHandSide
      caseChildren@(_, exprId', _, _, _) = compileView children (exprId + 1) (Context (scope, variableStack' ++ variableStack)) parent predecessors
      (nextPatterns, exprId'') = getMatchPatterns cases internalVariableName exprId' context parent predecessors
   in ((conditions, caseChildren) : nextPatterns, exprId'')

getCaseCondition :: Int -> [[Indent]] -> [Indent]
getCaseCondition index [] = [Ln "(() => {throw new Error(\"No matching pattern found\")})();"]
getCaseCondition index ([] : restConditions) = [Ln (show index ++ ";")]
getCaseCondition index (currentConditions : restConditions) = intersperse (Ln " && ") currentConditions ++ [Ln (" ? " ++ show index ++ " : "), Br] ++ getCaseCondition (index + 1) restConditions

type Child = String

appendChild :: Parent -> [Predecessor] -> Child -> String
appendChild parent [] child = parent ++ ".prepend(" ++ child ++ ");"
appendChild _ ((Predecessor predecessor) : ps) child = predecessor ++ ".after(" ++ child ++ ");"

predecessorChain :: [Predecessor] -> String
predecessorChain (Predecessor predecessor : ps) = predecessor
predecessorChain (MaybePredecessor predecessor : ps) = predecessor ++ " || " ++ predecessorChain ps
predecessorChain [] = "null"