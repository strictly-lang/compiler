module Compiler.Types.View.Base (compileView) where

import Compiler.Types
import Compiler.Types.View.Host (compileHost)
import Compiler.Util (functionToJs, getGetFreshExprId, indent, leftHandSideToJs, propertyChainToString, rightHandSideValueFunctionCallToJs, rightHandSideValueToJs)
import Data.List (intercalate, intersect, intersperse, isPrefixOf, partition)
import Types

type Successor = String

type Parent = String

type CompileResult = ([Indent], [Predecessor], UpdateCallbacks, RemoveCallbacks)

compileView :: [ViewContent] -> Context -> Parent -> [Predecessor] -> AppStateMonad CompileResult
compileView [] context _ predecessors = do return ([], predecessors, UpdateCallbacks [], RemoveCallbacks [])
compileView ((MixedText [] : ns)) context@(Context (scope, variableStack)) parent predecessors = compileView ns context parent predecessors
compileView (((MixedText (StaticText staticText : nextTexts)) : ns)) context@(Context (scope, variableStack)) parent predecessors =
  do
    exprId <- getGetFreshExprId
    let elementVariable = propertyChainToString scope ++ ".el" ++ show exprId

    (successorContent, predecessors', successorUpdateCallbacks, RemoveCallbacks successorRemoveCallback) <- compileView (MixedText nextTexts : ns) context parent (Predecessor elementVariable : predecessors)
    return
      ( Ln (elementVariable ++ " = document.createTextNode(\"" ++ staticText ++ "\");") :
        Br :
        Ln (appendChild parent predecessors elementVariable) :
        Br :
        successorContent,
        predecessors',
        successorUpdateCallbacks,
        RemoveCallbacks ([Ln (elementVariable ++ ".remove();"), Br] ++ successorRemoveCallback)
      )
compileView (((MixedText (DynamicText rightHandSideValue : nextTexts)) : ns)) context@(Context (scope, variableStack)) parent predecessors =
  do
    exprId <- getGetFreshExprId
    let elementVariable = propertyChainToString scope ++ ".el" ++ show exprId
        (rightHandValueJs, rightHandDependencies) = rightHandSideValueToJs variableStack rightHandSideValue

    (successorContent, predecessors', UpdateCallbacks successorUpdateCallbacks, RemoveCallbacks successorRemoveCallback) <- compileView (MixedText nextTexts : ns) context parent (Predecessor elementVariable : predecessors)
    return
      ( Ln (elementVariable ++ " = document.createTextNode(") :
        rightHandValueJs
          ++ [ Ln ");",
               Br,
               Ln (appendChild parent predecessors elementVariable),
               Br
             ]
          ++ successorContent,
        predecessors',
        UpdateCallbacks
          ( [ ( dependency,
                [Ln (elementVariable ++ ".textContent = ")] ++ rightHandValueJs ++ [Ln ";", Br]
              )
              | dependency <- rightHandDependencies
            ]
              ++ successorUpdateCallbacks
          ),
        RemoveCallbacks ([Ln (elementVariable ++ ".remove();"), Br] ++ successorRemoveCallback)
      )
compileView ((Host host importPath) : ns) context@(Context (scope, variableStack)) parent predecessors =
  do
    exprId <- getGetFreshExprId
    let elementVariable = propertyChainToString scope ++ ".el" ++ show exprId
    (HostElement (nodeName, options, children), hostSpecificContent, UpdateCallbacks hostSpecificUpdateCallbacks) <- compileHost context elementVariable host importPath
    (childrenContent, _, UpdateCallbacks childrenUpdateCallbacks, _) <- compileView children context elementVariable []
    (successorContent, predecessors', UpdateCallbacks successorUpdateCallbacks, RemoveCallbacks successorRemoveCallbacks) <- compileView ns context parent (Predecessor elementVariable : predecessors)
    let getAttributeValue = \attributeRightHandSide -> ([rightHandSideValueToJs variableStack singleAttributeRightHandSide | RightHandSideValue singleAttributeRightHandSide <- attributeRightHandSide])

    return
      ( [Ln (elementVariable ++ " = document.createElement(\"" ++ nodeName ++ "\");"), Br]
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
compileView ((ViewModel (leftHandSide, sourceValue) children) : ns) context@(Context (scope, variableStack)) parent predecessors =
  do
    exprId <- getGetFreshExprId
    let modelScope = scope ++ [DotNotation ("model" ++ show exprId)]
        modeledVariableStack = snd (leftHandSideToJs variableStack leftHandSide modelScope) ++ variableStack
    (childrenContent, predecessors', UpdateCallbacks childrenUpdateCallbacks, removeCallbacks) <- compileView children (Context (scope, modeledVariableStack)) parent predecessors
    let (modelUpdateCallback, restUpdateCallbacks) = partition (isPrefixOf modelScope . fst) childrenUpdateCallbacks
        modelUpdateCallbackJs =
          [ Ln "() => {",
            Br,
            Ind (intercalate [Br] (map snd modelUpdateCallback)),
            Br,
            Ln "}"
          ]
        (modelValue, modelDependencies) = rightHandSideValueFunctionCallToJs [modelUpdateCallbackJs] variableStack sourceValue
    (successorContent, predecessors'', UpdateCallbacks successorUpdateCallbacks, RemoveCallbacks successorRemoveCallbacks) <- compileView ns context parent predecessors'

    return
      ( [Ln (propertyChainToString modelScope ++ " = ")] ++ modelValue ++ [Ln ";", Br]
          ++ childrenContent
          ++ successorContent,
        predecessors'',
        UpdateCallbacks ([(modelDependency, []) | modelDependency <- modelDependencies] ++ restUpdateCallbacks),
        removeCallbacks
      )
compileView ((Each (leftHandSideValue, sourceValue) entityChildren negativeChildren) : ns) context@(Context (scope, variableStack)) parent predecessors =
  do
    exprId <- getGetFreshExprId
    let indexVariable = "index" ++ show exprId
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
    (entityChildrenContent, entitySuccessor, UpdateCallbacks entityChildrenUpdateCallbacks, RemoveCallbacks positiveRemoveCallbacks) <- compileView entityChildren (Context (entityScope, entityStack ++ variableStack)) parent (Predecessor entityPredecessor : predecessors)
    (negativeChildrenContent, negativeSuccessor, UpdateCallbacks negativeChildrenUpdateCallbacks, RemoveCallbacks negativeRemoveCallbacks) <- compileView negativeChildren context parent predecessors
    (successorContent, predecessors', UpdateCallbacks successorUpdateCallbacks, RemoveCallbacks successorRemoveCallbacks) <- compileView ns context parent [Predecessor successor]
    let (entityUpdateCallback, restEntityUpdateCallbacks) = partition (isPrefixOf entityValue . fst) entityChildrenUpdateCallbacks

    return
      ( [ Ln (propertyChainToString entitiesScope ++ " = [];"),
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
compileView ((Condition conditionValue positiveChildren negativeChildren) : ns) context@(Context (scope, variableStack)) parent predecessors =
  do
    exprId <- getGetFreshExprId
    let conditionVariable = propertyChainToString scope ++ ".condition" ++ show exprId
    (positiveChildrenContent, positiveSuccessor, UpdateCallbacks positiveChildrenUpdateCallbacks, RemoveCallbacks positiveRemoveCallbacks) <- compileView positiveChildren context parent predecessors
    (negativeChildrenContent, negativeSuccessor, UpdateCallbacks negativeChildrenUpdateCallbacks, RemoveCallbacks negativeRemoveCallbacks) <- compileView negativeChildren context parent predecessors
    let successor = "(" ++ conditionVariable ++ " ? " ++ predecessorChain positiveSuccessor ++ " : " ++ predecessorChain negativeSuccessor ++ ")"
        (internalConditionValue, conditionValueDependencies) = rightHandSideValueToJs variableStack conditionValue
        createPositiveCallback = propertyChainToString scope ++ ".createPositive" ++ show exprId
        createNegativeCallback = propertyChainToString scope ++ ".createNegative" ++ show exprId
        removeCallback = propertyChainToString scope ++ ".removeCallback" ++ show exprId
        createCallback = "createCondition" ++ show exprId
        updateCallback = propertyChainToString scope ++ ".updateCondition" ++ show exprId
    (successorContent, predecessors', UpdateCallbacks successorUpdateCallbacks, RemoveCallbacks successorRemoveCallbacks) <- compileView ns context parent (Predecessor successor : predecessors)

    return
      ( [ Ln (createPositiveCallback ++ " = () => {"),
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
compileView ((Match rightHandValue cases : ns)) context@(Context (scope, variableStack)) parent predecessors =
  do
    exprId <- getGetFreshExprId
    let currentValueVariable = scope ++ [DotNotation ("currentValue" ++ show exprId)]
        currentCaseVariable = propertyChainToString scope ++ ".currentCase" ++ show exprId
        updateCallback = propertyChainToString scope ++ ".updateCallback" ++ show exprId
        (rightHandValueJs, dependencies) = rightHandSideValueToJs variableStack rightHandValue
    patterns <- getMatchPatterns cases currentValueVariable context parent predecessors
    let updateCases = map (partition (isPrefixOf currentValueVariable . fst) . (\(_, (_, _, UpdateCallbacks updateCallbacks, _)) -> updateCallbacks)) patterns
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
    (successorContent, predecessors', UpdateCallbacks successorUpdateCallbacks, RemoveCallbacks successorRemoveCallback) <- compileView ns context parent (getCaseSuccessor currentCaseVariable 0 (map snd patterns))

    return
      ( [ Ln (propertyChainToString currentValueVariable ++ " = undefined;"),
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
                                   | ((_, (_, _, _, RemoveCallbacks removeCallbacks)), index) <- zip patterns [0 ..]
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
                               | ((_, (caseContent, _, _, _)), index) <- zip patterns [0 ..]
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

getMatchPatterns :: [Case] -> InternalVariableName -> Context -> Parent -> [Predecessor] -> AppStateMonad [([Indent], CompileResult)]
getMatchPatterns [] internalVariableName context parent predecessors =
  do
    return []
getMatchPatterns ((Case leftHandSide children) : cases) internalVariableName context@(Context (scope, variableStack)) parent predecessors =
  do
    let (conditions, variableStack') = leftHandSideToJs variableStack leftHandSide internalVariableName
    caseChildren@(_, _, _, _) <- compileView children (Context (scope, variableStack' ++ variableStack)) parent predecessors
    nextPatterns <- getMatchPatterns cases internalVariableName context parent predecessors

    return ((conditions, caseChildren) : nextPatterns)

getCaseSuccessor :: String -> Index -> [CompileResult] -> [Predecessor]
getCaseSuccessor currentCaseVariable index [] = [Predecessor "(() => {throw new Error(\"Could not find pattern\")})"]
getCaseSuccessor currentCaseVariable index ((_, Predecessor currentPredecessors : np, _, _) : restCaseResult) =
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