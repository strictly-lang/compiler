module Compiler.Types.View.Base (compileView) where

import Compiler.Types
import Compiler.Types.View.Host (compileHost)
import Compiler.Util (functionToJs, getGetFreshExprId, indent, leftHandSideToJs, propertyChainToString, rightHandSideValueFunctionCallToJs, rightHandSideValueToJs)
import Data.Char (toUpper)
import Data.List (intercalate, intersect, intersperse, isPrefixOf, partition)
import Types

type Parent = String

compileView :: [ViewContent] -> Context -> Parent -> [Predecessor] -> AppStateMonad CompileResult
compileView [] context _ predecessors = do return (CompileResult {compileCreate = [], compilePredecessors = predecessors, compileUpdate = [], compileRemove = []})
compileView ((MixedText [] : ns)) context@(Context (scope, variableStack)) parent predecessors = compileView ns context parent predecessors
compileView (((MixedText (StaticText staticText : nextTexts)) : ns)) context@(Context (scope, variableStack)) parent predecessors =
  do
    exprId <- getGetFreshExprId
    let elementVariable = propertyChainToString scope ++ ".el" ++ show exprId

    successor <- compileView (MixedText nextTexts : ns) context parent (Predecessor elementVariable : predecessors)

    return
      ( CompileResult
          { compileCreate =
              Ln (elementVariable ++ " = document.createTextNode(\"" ++ staticText ++ "\");") :
              Br :
              Ln (appendChild parent predecessors elementVariable) :
              Br :
              compileCreate successor,
            compilePredecessors = compilePredecessors successor,
            compileUpdate = compileUpdate successor,
            compileRemove = [Ln (elementVariable ++ ".remove();"), Br] ++ compileRemove successor
          }
      )
compileView (((MixedText (DynamicText rightHandSideValue : nextTexts)) : ns)) context@(Context (scope, variableStack)) parent predecessors =
  do
    exprId <- getGetFreshExprId
    let elementVariable = propertyChainToString scope ++ ".el" ++ show exprId
        (rightHandValueJs, rightHandDependencies) = rightHandSideValueToJs variableStack rightHandSideValue

    successor <- compileView (MixedText nextTexts : ns) context parent (Predecessor elementVariable : predecessors)

    return
      ( CompileResult
          { compileCreate =
              Ln (elementVariable ++ " = document.createTextNode(") :
              rightHandValueJs
                ++ [ Ln ");",
                     Br,
                     Ln (appendChild parent predecessors elementVariable),
                     Br
                   ]
                ++ compileCreate successor,
            compilePredecessors = compilePredecessors successor,
            compileUpdate =
              [ ( dependency,
                  [Ln (elementVariable ++ ".textContent = ")] ++ rightHandValueJs ++ [Ln ";", Br]
                )
                | dependency <- rightHandDependencies
              ]
                ++ compileUpdate successor,
            compileRemove = [Ln (elementVariable ++ ".remove();"), Br] ++ compileRemove successor
          }
      )
compileView ((Host host importPath) : ns) context@(Context (scope, variableStack)) parent predecessors =
  do
    exprId <- getGetFreshExprId
    let elementVariable = propertyChainToString scope ++ ".el" ++ show exprId
    (HostElement (nodeName, options, children), hostSpecificContent, hostSpecificUpdateCallbacks) <- compileHost context elementVariable host importPath
    childrenResult <- compileView children context elementVariable []
    successor <- compileView ns context parent (Predecessor elementVariable : predecessors)
    let getAttributeValue = \attributeRightHandSide -> ([rightHandSideValueToJs variableStack singleAttributeRightHandSide | RightHandSideValue singleAttributeRightHandSide <- attributeRightHandSide])
    let isCustomElement = '-' `elem` nodeName
    return
      ( CompileResult
          { compileCreate =
              [Ln (elementVariable ++ " = document.createElement(\"" ++ nodeName ++ "\");"), Br]
                ++ concat
                  [ if "on" `isPrefixOf` attributeKey
                      then [Ln (elementVariable ++ ".addEventListener(\"" ++ drop 2 attributeKey ++ "\", ")] ++ fst (functionToJs variableStack attributeRightHandSide) ++ [Ln ");", Br]
                      else
                        let value = concatMap fst (getAttributeValue attributeRightHandSide)
                         in if isCustomElement -- strictly custom-elements expect properties instead of attributes
                              then Ln (elementVariable ++ "." ++ attributeKey ++ " = ") : value ++ [Ln ";", Br]
                              else Ln (elementVariable ++ ".setAttribute(\"" ++ attributeKey ++ "\", ") : value ++ [Ln ");", Br]
                    | (attributeKey, attributeRightHandSide) <- options
                  ]
                ++ [ Ln (appendChild parent predecessors elementVariable),
                     Br
                   ]
                ++ hostSpecificContent
                ++ compileCreate childrenResult
                ++ compileCreate successor,
            compilePredecessors = compilePredecessors successor,
            compileUpdate =
              hostSpecificUpdateCallbacks
                ++ [ ( dependency,
                       if isCustomElement -- strictly custom-elements expect properties instead of attributes
                         then Ln (elementVariable ++ "." ++ attributeKey ++ " = ") : attributeJs ++ [Ln ";", Br]
                         else Ln (elementVariable ++ ".setAttribute(\"" ++ attributeKey ++ "\", ") : attributeJs ++ [Ln ");", Br]
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
                ++ compileUpdate childrenResult
                ++ compileUpdate successor,
            compileRemove = Ln (elementVariable ++ ".remove();") : compileRemove successor
          }
      )
compileView ((ViewModel (leftHandSide, sourceValue) children) : ns) context@(Context (scope, variableStack)) parent predecessors =
  do
    exprId <- getGetFreshExprId
    let modelScope = scope ++ [DotNotation ("model" ++ show exprId)]
        modeledVariableStack = snd (leftHandSideToJs variableStack leftHandSide modelScope) ++ variableStack
    childrenResult <- compileView children (Context (scope, modeledVariableStack)) parent predecessors
    let (modelUpdateCallback, restUpdateCallbacks) = partition (isPrefixOf modelScope . fst) (compileUpdate childrenResult)
        modelUpdateCallbackJs =
          [ Ln "() => {",
            Br,
            Ind (intercalate [Br] (map snd modelUpdateCallback)),
            Br,
            Ln "}"
          ]
        (modelValue, modelDependencies) = rightHandSideValueFunctionCallToJs [modelUpdateCallbackJs] variableStack sourceValue
    successor <- compileView ns context parent (compilePredecessors childrenResult)

    return
      ( CompileResult
          { compileCreate =
              [Ln (propertyChainToString modelScope ++ " = ")] ++ modelValue ++ [Ln ";", Br]
                ++ compileCreate childrenResult
                ++ compileCreate successor,
            compilePredecessors = compilePredecessors successor,
            compileUpdate =
              [ ( modelDependency,
                  [Ln (propertyChainToString modelScope ++ " = ")] ++ modelValue ++ [Ln ";", Br] ++ concatMap snd modelUpdateCallback
                )
                | modelDependency <- modelDependencies
              ]
                ++ restUpdateCallbacks,
            compileRemove =
              [ Ln ("delete " ++ propertyChainToString modelScope ++ ";"),
                Br
              ]
                ++ compileRemove childrenResult
          }
      )
compileView ((Each (leftHandSideValue, sourceValue) entityChildren negativeChildren) : ns) context@(Context (scope, variableStack)) parent predecessors =
  do
    exprId <- getGetFreshExprId
    let indexVariable = "index" ++ show exprId
        entitiesVariable = scope ++ [DotNotation ("entities" ++ show exprId)]
        entityVariable = entitiesVariable ++ [BracketNotation indexVariable]
        (conditions, entityStack) = leftHandSideToJs variableStack leftHandSideValue entityVariable
        entitiesScope = scope ++ [DotNotation ("nestedEntities" ++ show exprId)]
        entityScope = entitiesScope ++ [BracketNotation indexVariable]
        getPredecessorOf = scope ++ [DotNotation ("getPredecessorOf" ++ show exprId)]
        lastIndex = scope ++ [DotNotation ("lastIndex" ++ show exprId)]
        removeCallback = scope ++ [DotNotation ("remove" ++ show exprId)]
        updateCallback = scope ++ [DotNotation ("update" ++ show exprId)]
        previousLength = "previousLength" ++ show exprId
    entityResult <- compileView entityChildren (Context (entityScope, entityStack ++ variableStack)) parent (Predecessor (propertyChainToString getPredecessorOf ++ "(" ++ indexVariable ++ ")") : predecessors)
    elseResult <- compileView negativeChildren context parent predecessors
    successor <- compileView ns context parent (Predecessor (propertyChainToString getPredecessorOf ++ "(" ++ propertyChainToString entitiesVariable ++ ".length)") : predecessors)
    let (entityUpdateCallback, restEntityUpdateCallbacks) = partition (isPrefixOf entityVariable . fst) (compileUpdate entityResult)
        (entitiesValue, entitiesDependencies) = rightHandSideValueToJs variableStack sourceValue

    return
      ( CompileResult
          { compileCreate =
              [ Ln (propertyChainToString entitiesScope ++ " = {};"),
                Br,
                Ln (propertyChainToString getPredecessorOf ++ " = (" ++ indexVariable ++ ") => {"),
                Br,
                Ind
                  [ Ln ("if(Object.keys(" ++ propertyChainToString entitiesScope ++ ").length === 0) {"),
                    Br,
                    Ind
                      [ Ln ("return " ++ predecessorChain (compilePredecessors elseResult) ++ ";")
                      ],
                    Br,
                    Ln ("} else if (" ++ indexVariable ++ " === 0) {"),
                    Br,
                    Ind
                      [ Ln ("return " ++ predecessorChain predecessors ++ ";")
                      ],
                    Br,
                    Ln ("} else if (" ++ indexVariable ++ "- 1 in " ++ propertyChainToString entitiesScope ++ ") {"),
                    Br,
                    Ind
                      [ Ln ("var " ++ indexVariable ++ " = " ++ indexVariable ++ " - 1;"), --TODO remove ugly *var*
                        Ln ("return " ++ predecessorChain (compilePredecessors entityResult) ++ ";")
                      ],
                    Br,
                    Ln "}",
                    Br,
                    Ln ("return " ++ propertyChainToString getPredecessorOf ++ "(" ++ indexVariable ++ " - 1);")
                  ],
                Br,
                Ln "}",
                Br,
                Ln (propertyChainToString removeCallback ++ " = (" ++ indexVariable ++ ") => {"),
                Br,
                Ind
                  ( compileRemove entityResult
                      ++ [ Br,
                           Ln ("delete " ++ propertyChainToString entityScope ++ ";")
                         ]
                  ),
                Br,
                Ln "};",
                Br,
                Ln (propertyChainToString updateCallback ++ " = () => {"),
                Br,
                Ind
                  ( [ Ln ("let " ++ previousLength ++ " = " ++ propertyChainToString entitiesVariable ++ "?.length;"),
                      Br,
                      Ln (propertyChainToString entitiesVariable ++ " = ")
                    ]
                      ++ entitiesValue
                      ++ [ Ln ";",
                           Br,
                           Ln
                             ("for (let " ++ indexVariable ++ " = 0; " ++ indexVariable ++ " < " ++ propertyChainToString entitiesVariable ++ ".length; " ++ indexVariable ++ "++) {"),
                           Br,
                           Ind
                             ( if null conditions
                                 then
                                   [ Ln ("if (" ++ indexVariable ++ " in " ++ propertyChainToString entitiesScope ++ ") {"),
                                     Br,
                                     Ind (concatMap snd entityUpdateCallback),
                                     Br,
                                     Ln "} else {",
                                     Br,
                                     Ind
                                       ( [ Ln (propertyChainToString entityScope ++ " = {}"),
                                           Br
                                         ]
                                           ++ compileCreate entityResult
                                       ),
                                     Br,
                                     Ln "}",
                                     Br
                                   ]
                                 else
                                   Ln "if (" :
                                   conditions
                                     ++ [ Ln ") {",
                                          Br,
                                          Ind
                                            [ Ln ("if (" ++ indexVariable ++ " in " ++ propertyChainToString entitiesScope ++ ") {"),
                                              Br,
                                              Ind (concatMap snd entityUpdateCallback),
                                              Br,
                                              Ln "} else {",
                                              Br,
                                              Ind
                                                ( [ Ln (propertyChainToString entityScope ++ " = {}"),
                                                    Br
                                                  ]
                                                    ++ compileCreate entityResult
                                                ),
                                              Br,
                                              Ln "}",
                                              Br
                                            ],
                                          Ln ("} else if (" ++ indexVariable ++ " in " ++ propertyChainToString entitiesScope ++ ") {"),
                                          Br,
                                          Ind
                                            [ Ln (propertyChainToString removeCallback ++ "(" ++ indexVariable ++ ");")
                                            ],
                                          Br,
                                          Ln "}",
                                          Br
                                        ]
                             ),
                           Br,
                           Ln "}",
                           Br,
                           Ln ("for (let " ++ indexVariable ++ " = " ++ previousLength ++ " - 1; " ++ indexVariable ++ " >= " ++ propertyChainToString entitiesVariable ++ ".length; " ++ indexVariable ++ "--) {"),
                           Br,
                           Ind
                             [ Ln (propertyChainToString removeCallback ++ "(" ++ indexVariable ++ ");")
                             ],
                           Br,
                           Ln "}",
                           Br,
                           Ln ("if (Object.keys(" ++ propertyChainToString entitiesScope ++ ").length === 0 && " ++ previousLength ++ " !== 0) {"),
                           Br,
                           Ind (compileCreate elseResult),
                           Ln ("} else if (Object.keys(" ++ propertyChainToString entitiesScope ++ ").length !== 0 && " ++ previousLength ++ " === 0) {"),
                           Br,
                           Ind (compileRemove elseResult),
                           Ln "}",
                           Br
                         ]
                  ),
                Br,
                Ln "}",
                Br,
                Ln (propertyChainToString updateCallback ++ "();"),
                Br
              ]
                ++ compileCreate successor,
            compilePredecessors = compilePredecessors successor,
            compileUpdate =
              [(dependency, [Ln (propertyChainToString updateCallback ++ "();"), Br]) | dependency <- entitiesDependencies]
                ++ [ ( dependency,
                       [ Ln ("for (let " ++ indexVariable ++ " = 0; " ++ indexVariable ++ " < " ++ propertyChainToString entitiesVariable ++ ".length; " ++ indexVariable ++ "++) {"),
                         Br,
                         Ind
                           [ Ln ("if (" ++ indexVariable ++ " in " ++ propertyChainToString entitiesScope ++ ") {"),
                             Br,
                             Ind restEntityUpdateCallback,
                             Br,
                             Ln "}",
                             Br
                           ],
                         Br,
                         Ln "}",
                         Br
                       ]
                     )
                     | (dependency, restEntityUpdateCallback) <- restEntityUpdateCallbacks
                   ]
                ++ [ ( dependency,
                       [ Ln ("if (" ++ propertyChainToString entitiesVariable ++ ".length === 0) {"),
                         Br,
                         Ind negativepdateCallback,
                         Ln "}",
                         Br
                       ]
                     )
                     | (dependency, negativepdateCallback) <- compileUpdate elseResult
                   ],
            compileRemove =
              [ Ln ("if (" ++ propertyChainToString entitiesVariable ++ ".length === 0) {"),
                Br,
                Ind (compileRemove elseResult),
                Br,
                Ln "} else {",
                Br,
                Ind
                  [ Ln ("for (let " ++ indexVariable ++ " = 0; " ++ indexVariable ++ " < " ++ propertyChainToString entitiesVariable ++ ".length; " ++ indexVariable ++ "++) {"),
                    Br,
                    Ind
                      [ Ln ("if (" ++ indexVariable ++ " in " ++ propertyChainToString entitiesScope ++ ") {"),
                        Br,
                        Ind [Ln (propertyChainToString removeCallback ++ "(" ++ indexVariable ++ ")")],
                        Br,
                        Ln "}",
                        Br
                      ],
                    Br,
                    Ln "}",
                    Br
                  ],
                Br,
                Ln "}",
                Br
              ]
          }
      )
compileView ((Condition conditionValue positiveChildren negativeChildren) : ns) context@(Context (scope, variableStack)) parent predecessors =
  do
    exprId <- getGetFreshExprId
    let localConditionVariable = "condition" ++ show exprId
    let conditionVariable = propertyChainToString scope ++ ".condition" ++ show exprId
    positiveChildrenResult <- compileView positiveChildren context parent predecessors
    negativeChildrenResult <- compileView negativeChildren context parent predecessors
    let successor = "(" ++ conditionVariable ++ " ? " ++ predecessorChain (compilePredecessors positiveChildrenResult) ++ " : " ++ predecessorChain (compilePredecessors negativeChildrenResult) ++ ")"
        (internalConditionValue, conditionValueDependencies) = rightHandSideValueToJs variableStack conditionValue
        createPositiveCallback = propertyChainToString scope ++ ".createPositive" ++ show exprId
        createNegativeCallback = propertyChainToString scope ++ ".createNegative" ++ show exprId
        removeCallback = propertyChainToString scope ++ ".removeCallback" ++ show exprId
        createCallback = "createCondition" ++ show exprId
        updateCallback = propertyChainToString scope ++ ".updateCondition" ++ show exprId
    successor <- compileView ns context parent (Predecessor successor : predecessors)

    return
      ( CompileResult
          { compileCreate =
              [ Ln (createPositiveCallback ++ " = () => {"),
                Br,
                Ind (compileCreate positiveChildrenResult),
                Ln "};",
                Br,
                Ln (createNegativeCallback ++ " = () => {"),
                Br,
                Ind (compileCreate negativeChildrenResult),
                Ln "};",
                Br,
                Ln (updateCallback ++ " = () => {"),
                Br,
                Ind
                  ( [Ln ("const " ++ localConditionVariable ++ " = ")]
                      ++ internalConditionValue
                      ++ [ Ln ";",
                           Br,
                           Ln ("if (" ++ localConditionVariable ++ " !== " ++ conditionVariable ++ ") {"),
                           Br,
                           Ind
                             [ Ln ("if (" ++ conditionVariable ++ " !== undefined) {"),
                               Ind
                                 [ Br,
                                   Ln (removeCallback ++ "();"),
                                   Br
                                 ],
                               Br,
                               Ln "}",
                               Br,
                               Ln (conditionVariable ++ " = " ++ localConditionVariable ++ ";"),
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
                               Ln "}"
                             ],
                           Br,
                           Ln "}",
                           Br
                         ]
                  ),
                Ln "};",
                Br,
                Ln (updateCallback ++ "();"),
                Br,
                Ln (removeCallback ++ " = () => {"),
                Br,
                Ind
                  [ Ln ("if (" ++ conditionVariable ++ ") {"),
                    Br,
                    Ind (compileRemove positiveChildrenResult),
                    Br,
                    Ln "} else {",
                    Br,
                    Ind (compileRemove negativeChildrenResult),
                    Br,
                    Ln "}",
                    Br
                  ],
                Ln "};",
                Br
              ]
                ++ compileCreate successor,
            compilePredecessors = compilePredecessors successor,
            compileUpdate =
              [(dependency, [Ln (updateCallback ++ "();")]) | dependency <- conditionValueDependencies]
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
                     | (internalVariableName, updateCallback) <- compileUpdate positiveChildrenResult
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
                     | (internalVariableName, updateCallback) <- compileUpdate negativeChildrenResult
                   ]
                ++ compileUpdate successor,
            compileRemove =
              [ Ln (removeCallback ++ "();"),
                Br,
                Ln ("delete " ++ conditionVariable ++ ";"),
                Br
              ]
                ++ compileRemove successor
          }
      )
compileView ((Match rightHandValue cases : ns)) context@(Context (scope, variableStack)) parent predecessors =
  do
    exprId <- getGetFreshExprId
    let currentValueVariable = scope ++ [DotNotation ("currentValue" ++ show exprId)]
        currentCaseVariable = propertyChainToString scope ++ ".currentCase" ++ show exprId
        previousCaseVariable = "previousCase" ++ show exprId
        updateCallback = propertyChainToString scope ++ ".updateCallback" ++ show exprId
        removeCallback = propertyChainToString scope ++ ".removeCallback" ++ show exprId
        (rightHandValueJs, dependencies) = rightHandSideValueToJs variableStack rightHandValue
    patterns <- getMatchPatterns cases currentValueVariable context parent predecessors
    let updateCases = map (partition (isPrefixOf currentValueVariable . fst) . (\(_, caseResult) -> compileUpdate caseResult)) patterns
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
    successor <- compileView ns context parent (getCaseSuccessor currentCaseVariable 0 (map snd patterns))

    return
      ( CompileResult
          { compileCreate =
              [ Ln (updateCallback ++ " = () => {"),
                Br,
                Ind
                  ( [ Ln ("const " ++ previousCaseVariable ++ " = " ++ currentCaseVariable ++ ";"),
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
                           Ln ("if (" ++ previousCaseVariable ++ " === " ++ currentCaseVariable ++ ") {"),
                           Br,
                           Ind
                             (concat (concat activeUpdates)),
                           Br,
                           Ln "} else {",
                           Br,
                           Ind
                             ( [ Ln ("if (" ++ previousCaseVariable ++ " !== undefined) {"),
                                 Br,
                                 Ind
                                   [ Ln (removeCallback ++ "(" ++ previousCaseVariable ++ ");")
                                   ],
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
                                       Ind (compileCreate caseResult),
                                       Br,
                                       Ln "}"
                                     ]
                                     | ((_, caseResult), index) <- zip patterns [0 ..]
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
                Br,
                Ln (removeCallback ++ "= (previousCase) => {"),
                Br,
                Ind
                  ( intercalate
                      [Br]
                      [ [ if index == 0
                            then Ln ""
                            else Ln " else ",
                          Ln ("if (previousCase === " ++ show index ++ ") {"),
                          Br,
                          Ind (compileRemove patternResult),
                          Br,
                          Ln "}"
                        ]
                        | ((_, patternResult), index) <- zip patterns [0 ..]
                      ]
                  ),
                Br,
                Ln "};",
                Br
              ]
                ++ compileCreate successor,
            compilePredecessors = compilePredecessors successor,
            compileUpdate =
              [ (dependency, [Ln (updateCallback ++ "()")])
                | dependency <- dependencies
              ]
                ++ concat restUpdates
                ++ compileUpdate successor,
            compileRemove =
              [ Ln (removeCallback ++ ("(" ++ currentCaseVariable ++ ");")),
                Br,
                Ln ("delete " ++ currentCaseVariable ++ ";"),
                Br,
                Ln ("delete " ++ propertyChainToString currentValueVariable ++ ";"),
                Br
              ]
                ++ compileRemove successor
          }
      )
compileView ((ViewContext (leftHandSide, contextName) children) : ns) context@(Context (scope, variableStack)) parent predecessors =
  do
    exprId <- getGetFreshExprId
    let contextUpdate = scope ++ [DotNotation ("updateContext" ++ show exprId)]
    let findParentFunction = "findParent" ++ show exprId
    let contextResult = scope ++ [DotNotation ("currentValue" ++ show exprId)]
    let contextValue = contextResult ++ [DotNotation "value"]
    let (leftHandSideJs, variableStack') = leftHandSideToJs variableStack leftHandSide contextValue
    childrenResult <- compileView children (Context (scope, variableStack' ++ variableStack)) parent []
    successor <- compileView ns context parent predecessors
    let getAttributeValue = \attributeRightHandSide -> ([rightHandSideValueToJs variableStack singleAttributeRightHandSide | RightHandSideValue singleAttributeRightHandSide <- attributeRightHandSide])
    let (contextUpdateChildren, restChildrenUpdateCallbacks) = partition (isPrefixOf contextResult . fst) (compileUpdate childrenResult)

    return
      ( CompileResult
          { compileCreate =
              [ Ln (propertyChainToString contextUpdate ++ " = (newContextValue) => {"),
                Br,
                Ind
                  ( [ Ln (propertyChainToString contextValue ++ " = newContextValue"),
                      Br
                    ]
                      ++ concatMap snd contextUpdateChildren
                  ),
                Br,
                Ln "}",
                Br,
                Ln ("const " ++ findParentFunction ++ " = (element) => {"),
                Br,
                Ind
                  [ Ln ("if (element.tagName === \"" ++ [toUpper char | char <- contextName] ++ "\") {"),
                    Br,
                    Ind
                      [ Ln ("return element._context(" ++ propertyChainToString contextUpdate ++ ");")
                      ],
                    Br,
                    Ln "}",
                    Br,
                    Ln "if (element.parentNode === null) {",
                    Br,
                    Ind
                      [ Ln "if (element instanceof ShadowRoot) {",
                        Br,
                        Ind
                          [ Ln ("return " ++ findParentFunction ++ "(element.host);")
                          ],
                        Br,
                        Ln "}",
                        Br,
                        Ln ("throw new Error(\"Could not find provider \\\"" ++ contextName ++ "\\\"\");")
                      ],
                    Br,
                    Ln "}",
                    Br,
                    Ln ("return " ++ findParentFunction ++ "(element.parentNode);"),
                    Br
                  ],
                Ln "};",
                Br,
                Ln (propertyChainToString contextResult ++ " = " ++ findParentFunction ++ "(" ++ parent ++ ")"),
                Br
              ]
                ++ compileCreate childrenResult
                ++ compileCreate successor,
            compilePredecessors = compilePredecessors successor,
            compileUpdate =
              restChildrenUpdateCallbacks
                ++ compileUpdate successor,
            compileRemove =
              [ Ln (propertyChainToString contextResult ++ ".disconnect();"),
                Br,
                Ln ("delete " ++ propertyChainToString contextResult ++ ";"),
                Br
              ]
                ++ compileRemove childrenResult
                ++ compileRemove successor
          }
      )

type Index = Int

getMatchPatterns :: [Case] -> InternalVariableName -> Context -> Parent -> [Predecessor] -> AppStateMonad [([Indent], CompileResult)]
getMatchPatterns [] internalVariableName context parent predecessors =
  do
    return []
getMatchPatterns ((Case leftHandSide children) : cases) internalVariableName context@(Context (scope, variableStack)) parent predecessors =
  do
    let (conditions, variableStack') = leftHandSideToJs variableStack leftHandSide internalVariableName
    caseChildren <- compileView children (Context (scope, variableStack' ++ variableStack)) parent predecessors
    nextPatterns <- getMatchPatterns cases internalVariableName context parent predecessors

    return ((conditions, caseChildren) : nextPatterns)

getCaseSuccessor :: String -> Index -> [CompileResult] -> [Predecessor]
getCaseSuccessor currentCaseVariable index [] = [Predecessor "(() => {throw new Error(\"Could not find pattern\")})"]
getCaseSuccessor currentCaseVariable index (caseResult : restCaseResult) =
  let ((Predecessor nextPredecessors) : nP) = getCaseSuccessor currentCaseVariable (index + 1) restCaseResult
   in [Predecessor ("(" ++ currentCaseVariable ++ " === " ++ show index ++ " ? " ++ (predecessorChain (compilePredecessors caseResult)) ++ " : " ++ nextPredecessors ++ ")")]

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