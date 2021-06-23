module Compiler.Types.View.Base (compileView) where

import Compiler.Types
import Compiler.Types.View.Host (compileHost)
import Compiler.Util (functionToJs, indent, leftHandSideToJs, propertyChainToString, rightHandSideValueFunctionCallToJs, rightHandSideValueToJs)
import Data.List (intercalate, intersect, intersperse, isPrefixOf, partition)
import Types

type Successor = String

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
  let elementVariableFactory = \exprId' -> propertyChainToString scope ++ ".el" ++ show exprId'
      predecessorFactory = \exprId' -> if exprId' == exprId then predecessors else [Predecessor (elementVariableFactory (exprId' - 1))]
      textContents =
        [ case text of
            DynamicText rightHandValue ->
              let elementVariable = elementVariableFactory exprId'
                  updateCallback = propertyChainToString scope ++ ".updateCallback" ++ show exprId'
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
compileView (host@Host {} : ns) exprId context@(Context (scope, variableStack)) parent predecessors =
  let elementVariable = propertyChainToString scope ++ ".el" ++ show exprId
      (Host nodeName options children, hostSpecificContent, UpdateCallbacks hostSpecificUpdateCallbacks) = compileHost context exprId elementVariable host
      (childrenContent, exprId', _, UpdateCallbacks childrenUpdateCallbacks, _) = compileView children (exprId + 1) context elementVariable []
      (successorContent, exprId'', predecessors', UpdateCallbacks successorUpdateCallbacks, RemoveCallbacks successorRemoveCallbacks) = compileView ns (exprId' + 1) context parent (Predecessor elementVariable : predecessors)
      getAttributeValue = \attributeRightHandSide -> ([rightHandSideValueToJs variableStack singleAttributeRightHandSide | RightHandSideValue singleAttributeRightHandSide <- attributeRightHandSide])
   in ( [Ln (elementVariable ++ " = document.createElement(\"" ++ nodeName ++ "\");"), Br]
          ++ concat
            [ if "on" `isPrefixOf` attributeKey
                then [Ln (elementVariable ++ ".addEventListener(\"" ++ drop 2 attributeKey ++ "\", ")] ++ fst (functionToJs variableStack attributeRightHandSide) ++ [Ln ");", Br]
                else Ln (elementVariable ++ ".setAttribute(\"" ++ attributeKey ++ "\", ") : concatMap fst (getAttributeValue attributeRightHandSide) ++ [Ln ");", Br]
              | (attributeKey, attributeRightHandSide) <- options
            ]
          ++ [ Ln (appendChild parent predecessors elementVariable),
               Br
             ]
          ++ hostSpecificContent
          ++ childrenContent
          ++ successorContent,
        exprId'',
        predecessors',
        UpdateCallbacks
          ( hostSpecificUpdateCallbacks
              ++ [ ( dependency,
                     Ln (elementVariable ++ ".setAttribute(\"" ++ attributeKey ++ "\", ") : attributeJs ++ [Ln ");", Br]
                   )
                   | (attributeKey, attributeRightHandSide) <- options,
                     (attributeJs, dependencies) <- getAttributeValue attributeRightHandSide,
                     dependency <- dependencies
                 ]
              ++ [ (dependency, [])
                   | (attributeKey, attributeRightHandSide) <- options,
                     attributeKey `isPrefixOf` "on",
                     dependency <- snd (functionToJs variableStack attributeRightHandSide)
                 ]
              ++ childrenUpdateCallbacks
              ++ successorUpdateCallbacks
          ),
        RemoveCallbacks (Ln (elementVariable ++ ".remove();") : successorRemoveCallbacks)
      )
compileView ((ViewModel (Expression (leftHandSide, FeedOperator, sourceValue)) children) : ns) exprId context@(Context (scope, variableStack)) parent predecessors =
  let modelScope = scope ++ [DotNotation ("model" ++ show exprId)]
      modeledVariableStack = snd (leftHandSideToJs variableStack leftHandSide modelScope) ++ variableStack
      (childrenContent, exprId', predecessors', UpdateCallbacks childrenUpdateCallbacks, removeCallbacks) = compileView children (exprId + 1) (Context (scope, modeledVariableStack)) parent predecessors
      (modelUpdateCallback, restUpdateCallbacks) = partition (isPrefixOf modelScope . fst) childrenUpdateCallbacks
      modelUpdateCallbackJs =
        [ Ln "() => {",
          Br,
          Ind (intercalate [Br] (map snd modelUpdateCallback)),
          Br,
          Ln "}"
        ]
      (modelValue, modelDependencies) = rightHandSideValueFunctionCallToJs [modelUpdateCallbackJs] variableStack sourceValue
      (successorContent, exprId''', predecessors'', UpdateCallbacks successorUpdateCallbacks, RemoveCallbacks successorRemoveCallbacks) = compileView ns (exprId' + 1) context parent predecessors'
   in ( [Ln (propertyChainToString modelScope ++ " = ")] ++ modelValue ++ [Ln ";", Br]
          ++ childrenContent
          ++ successorContent,
        exprId',
        predecessors'',
        UpdateCallbacks ([(modelDependency, []) | modelDependency <- modelDependencies] ++ restUpdateCallbacks),
        removeCallbacks
      )
compileView ((Each [Expression (leftHandSideValue, FeedOperator, sourceValue)] entityChildren negativeChildren) : ns) exprId context@(Context (scope, variableStack)) parent predecessors =
  let indexVariable = ("index" ++ show exprId)
      entitiesScope = scope ++ [DotNotation ("entities" ++ show exprId)]
      entityScope = entitiesScope ++ [BracketNotation indexVariable]
      entityValue = entityScope ++ [DotNotation "value"]
      updateCallback = propertyChainToString scope ++ ".updateCallback" ++ show exprId
      (internalEntitiesVariable, sourceValueDependencies) = rightHandSideValueToJs variableStack sourceValue
      createEntityCallback = propertyChainToString scope ++ ".createEach" ++ show exprId
      createElseCallback = propertyChainToString scope ++ ".createElse" ++ show exprId
      predecessorOf = propertyChainToString scope ++ ".getPredecessorOf" ++ show exprId
      successor = predecessorOf ++ "(" ++ propertyChainToString entitiesScope ++ ".length - 1)"
      entityPredecessor = predecessorOf ++ "(" ++ indexVariable ++ " - 1)"
      entityVariable = "entity" ++ show exprId
      (_conditions, entityStack) = leftHandSideToJs variableStack leftHandSideValue entityValue
      (entityChildrenContent, exprId', entitySuccessor, UpdateCallbacks entityChildrenUpdateCallbacks, RemoveCallbacks positiveRemoveCallbacks) = compileView entityChildren (exprId + 1) (Context (entityScope, entityStack ++ variableStack)) parent (Predecessor entityPredecessor : predecessors)
      (negativeChildrenContent, exprId'', negativeSuccessor, UpdateCallbacks negativeChildrenUpdateCallbacks, RemoveCallbacks negativeRemoveCallbacks) = compileView negativeChildren (exprId' + 1) context parent predecessors
      (successorContent, exprId''', predecessors', UpdateCallbacks successorUpdateCallbacks, RemoveCallbacks successorRemoveCallbacks) = compileView ns (exprId'' + 1) context parent [Predecessor successor]
      (entityUpdateCallback, restEntityUpdateCallbacks) = partition (isPrefixOf entityValue . fst) entityChildrenUpdateCallbacks
   in ( [ Ln (propertyChainToString entitiesScope ++ " = [];"),
          Br,
          Ln (predecessorOf ++ " = (" ++ indexVariable ++ ") => {"),
          Br,
          Ind
            [ Ln ("if (" ++ propertyChainToString entitiesScope ++ ".length === 0) {"),
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
                 [ Ln (propertyChainToString entitiesScope ++ ".push({value : " ++ entityVariable ++ "});"),
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
                     Ln ("const previousLength = " ++ propertyChainToString entitiesScope ++ ".length"),
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
                              Ln ("if (" ++ indexVariable ++ " < " ++ propertyChainToString entitiesScope ++ ".length) {"),
                              Br,
                              Ind
                                ( Ln (propertyChainToString entityValue ++ " = " ++ entityVariable ++ ";") :
                                  Br :
                                  concatMap
                                    snd
                                    entityUpdateCallback
                                ),
                              Ln "} else {",
                              Br,
                              Ind
                                [ Ln (propertyChainToString entitiesScope ++ ".push({value : " ++ entityVariable ++ "});"),
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
                          Ln ("for (let " ++ indexVariable ++ " = " ++ propertyChainToString entitiesScope ++ ".length - 1; " ++ indexVariable ++ " >= newCount; " ++ indexVariable ++ "--) {"),
                          Br,
                          Ind
                            ( concatMap (\positiveRemoveCallback -> positiveRemoveCallback : [Br]) positiveRemoveCallbacks
                                ++ [Ln (propertyChainToString entitiesScope ++ ".pop();"), Br]
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
                     [ Ln ("if( " ++ propertyChainToString entitiesScope ++ ".length === 0 ) {"),
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
  let conditionVariable = propertyChainToString scope ++ ".condition" ++ show exprId
      (positiveChildrenContent, exprId', positiveSuccessor, UpdateCallbacks positiveChildrenUpdateCallbacks, RemoveCallbacks positiveRemoveCallbacks) = compileView positiveChildren (exprId + 1) context parent predecessors
      (negativeChildrenContent, exprId'', negativeSuccessor, UpdateCallbacks negativeChildrenUpdateCallbacks, RemoveCallbacks negativeRemoveCallbacks) = compileView negativeChildren (exprId' + 1) context parent predecessors
      successor = "(" ++ conditionVariable ++ " ? " ++ predecessorChain positiveSuccessor ++ " : " ++ predecessorChain negativeSuccessor ++ ")"
      (internalConditionValue, conditionValueDependencies) = rightHandSideValueToJs variableStack conditionValue
      createPositiveCallback = propertyChainToString scope ++ ".createPositive" ++ show exprId
      createNegativeCallback = propertyChainToString scope ++ ".createNegative" ++ show exprId
      removeCallback = propertyChainToString scope ++ ".removeCallback" ++ show exprId
      createCallback = "createCondition" ++ show exprId
      updateCallback = propertyChainToString scope ++ ".updateCondition" ++ show exprId
      (successorContent, exprId''', predecessors', UpdateCallbacks successorUpdateCallbacks, RemoveCallbacks successorRemoveCallbacks) = compileView ns (exprId'' + 1) context parent (Predecessor successor : predecessors)
   in ( [ Ln (createPositiveCallback ++ " = () => {"),
          Br,
          Ind positiveChildrenContent,
          Ln "}",
          Br,
          Ln (createNegativeCallback ++ " = () => {"),
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
  let currentValueVariable = scope ++ [DotNotation ("currentValue" ++ show exprId)]
      currentCaseVariable = propertyChainToString scope ++ ".currentCase" ++ show exprId
      updateCallback = propertyChainToString scope ++ ".updateCallback" ++ show exprId
      (rightHandValueJs, dependencies) = rightHandSideValueToJs variableStack rightHandValue
      (patterns, exprId') = getMatchPatterns cases currentValueVariable (exprId + 1) context parent predecessors
      updateCases = map (partition (isPrefixOf currentValueVariable . fst) . (\(_, (_, _, _, UpdateCallbacks updateCallbacks, _)) -> updateCallbacks)) patterns
      activeUpdates =
        zipWith
          ( curry
              ( \((activeUpdates, _), index) ->
                  map
                    ( \(_, updateCode) ->
                        [ Ln
                            ("if (" ++ currentCaseVariable ++ " == " ++ show index ++ ") {"),
                          Br,
                          Ind updateCode,
                          Br,
                          Ln "}",
                          Br
                        ]
                    )
                    activeUpdates
              )
          )
          updateCases
          [0 ..]

      restUpdates =
        zipWith
          ( curry
              ( \((_, restUpdates), index) ->
                  map
                    ( \(internalVariableName, updateCode) ->
                        ( internalVariableName,
                          [ Ln
                              ("if (" ++ currentCaseVariable ++ " == " ++ show index ++ ") {"),
                            Br,
                            Ind updateCode,
                            Br,
                            Ln "}",
                            Br
                          ]
                        )
                    )
                    restUpdates
              )
          )
          updateCases
          [0 ..]
      (successorContent, exprId'', predecessors', UpdateCallbacks successorUpdateCallbacks, RemoveCallbacks successorRemoveCallback) = compileView ns exprId' context parent (getCaseSuccessor currentCaseVariable 0 (map snd patterns))
   in ( [ Ln (propertyChainToString currentValueVariable ++ " = undefined;"),
          Br,
          Ln (currentCaseVariable ++ " = undefined;"),
          Br,
          Ln (updateCallback ++ " = () => {"),
          Br,
          Ind
            ( [ Ln ("const previousCase = " ++ currentCaseVariable ++ ";"),
                Br,
                Ln (propertyChainToString currentValueVariable ++ " = ")
              ]
                ++ rightHandValueJs
                ++ [ Ln ";",
                     Br,
                     Ln (currentCaseVariable ++ " = "),
                     Br,
                     Ind (getCaseCondition 0 (map fst patterns)),
                     Br,
                     Ln ("if (previousCase === " ++ currentCaseVariable ++ ") {"),
                     Br,
                     Ind
                       (concat (concat activeUpdates)),
                     Br,
                     Ln "} else {",
                     Br,
                     Ind
                       ( [ Ln "if (previousCase !== undefined) {",
                           Br,
                           Ind
                             ( intercalate
                                 [Br]
                                 [ [ if index == 0
                                       then Ln ""
                                       else Ln " else ",
                                     Ln ("if (previousCase === " ++ show index ++ ") {"),
                                     Br,
                                     Ind removeCallbacks,
                                     Br,
                                     Ln "}"
                                   ]
                                   | ((_, (_, _, _, _, RemoveCallbacks removeCallbacks)), index) <- zip patterns [0 ..]
                                 ]
                             ),
                           Br,
                           Ln "}",
                           Br
                         ]
                           ++ intercalate
                             [Br]
                             [ [ if index == 0
                                   then Ln ""
                                   else Ln " else ",
                                 Ln ("if (" ++ currentCaseVariable ++ " === " ++ show index ++ ") {"),
                                 Br,
                                 Ind caseContent,
                                 Br,
                                 Ln "}"
                               ]
                               | ((_, (caseContent, _, _, _, _)), index) <- zip patterns [0 ..]
                             ]
                       ),
                     Br,
                     Ln "}",
                     Br
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
        UpdateCallbacks
          ( [ (dependency, [Ln (updateCallback ++ "()")])
              | dependency <- dependencies
            ]
              ++ concat restUpdates
              ++ successorUpdateCallbacks
          ),
        RemoveCallbacks successorRemoveCallback
      )

type Index = Int

getMatchPatterns :: [Case] -> InternalVariableName -> ExprId -> Context -> Parent -> [Predecessor] -> ([([Indent], CompileResult)], ExprId)
getMatchPatterns [] internalVariableName exprId context parent predecessors = ([], exprId)
getMatchPatterns ((Case leftHandSide children) : cases) internalVariableName exprId context@(Context (scope, variableStack)) parent predecessors =
  let (conditions, variableStack') = leftHandSideToJs variableStack leftHandSide internalVariableName
      caseChildren@(_, exprId', _, _, _) = compileView children (exprId + 1) (Context (scope, variableStack' ++ variableStack)) parent predecessors
      (nextPatterns, exprId'') = getMatchPatterns cases internalVariableName exprId' context parent predecessors
   in ((conditions, caseChildren) : nextPatterns, exprId'')

getCaseSuccessor :: String -> Index -> [CompileResult] -> [Predecessor]
getCaseSuccessor currentCaseVariable index [] = [Predecessor "(() => {throw new Error(\"Could not find pattern\")})"]
getCaseSuccessor currentCaseVariable index ((_, _, Predecessor currentPredecessors : np, _, _) : restCaseResult) =
  let ((Predecessor nextPredecessors) : nP) = getCaseSuccessor currentCaseVariable (index + 1) restCaseResult
   in [Predecessor ("(" ++ currentCaseVariable ++ " === " ++ show index ++ " ? " ++ currentPredecessors ++ " : " ++ nextPredecessors ++ ")")]

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
predecessorChain [] = "null"