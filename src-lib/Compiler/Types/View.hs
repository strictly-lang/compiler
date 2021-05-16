module Compiler.Types.View (compileView) where

import Compiler.Types
import Compiler.Util (filter', indent, publicVariableToInternal)
import Data.List (intercalate)
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
                  rightHandJsValue = rightHandSideToJs variableStack rightHandValue
               in ( [ Ln (elementVariable ++ " =  document.createTextNode(" ++ fst rightHandJsValue ++ ");"),
                      Ln (appendChild parent (predecessorFactory exprId') elementVariable),
                      Ln (removeCallback ++ " = () => " ++ elementVariable ++ ".remove()")
                    ],
                    [ ( dependency,
                        [Ln (elementVariable ++ ".textContent = " ++ fst rightHandJsValue ++ ";")]
                      )
                      | dependency <- snd rightHandJsValue
                    ],
                    [Ln (removeCallback ++ "();")]
                  )
            StaticText content ->
              let elementVariable = elementVariableFactory exprId'
                  removeCallback = scope ++ ".removeCallback" ++ show exprId'
               in ( [ Ln (elementVariable ++ " =  document.createTextNode(\"" ++ content ++ "\");"),
                      Ln (appendChild parent (predecessorFactory exprId') elementVariable),
                      Ln (removeCallback ++ " = () => " ++ elementVariable ++ ".remove()")
                    ],
                    [],
                    [Ln (removeCallback ++ "();")]
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
   in ( [Ln (elementVariable ++ " =  document.createElement(\"" ++ nodeName ++ "\");")]
          ++ [Ln (elementVariable ++ ".setAttribute(\"" ++ attributeKey ++ "\", " ++ fst (rightHandSideToJs variableStack attributeRightHandSide) ++ ")") | (attributeKey, attributeRightHandSide) <- options]
          ++ [ Ln (appendChild parent predecessors elementVariable),
               Ln (removeCallback ++ " = () => " ++ elementVariable ++ ".remove();")
             ]
          ++ childrenContent
          ++ successorContent,
        exprId'',
        predecessors',
        UpdateCallbacks ([(dependency, [
          Ln (elementVariable ++ ".setAttribute(\"" ++ attributeKey ++ "\", " ++ fst (rightHandSideToJs variableStack attributeRightHandSide) ++ ")")
        ]) | (attributeKey, attributeRightHandSide) <- options, dependency <- snd (rightHandSideToJs variableStack attributeRightHandSide)] ++ childrenUpdateCallbacks ++ successorUpdateCallbacks),
        RemoveCallbacks (Ln (removeCallback ++ "();") : successorRemoveCallbacks)
      )
compileView ((Each [Expression (LeftTuple [LeftVariable publicEntityVariable, LeftVariable publicIndexVariable], FeedOperator, sourceValue)] entityChildren negativeChildren) : ns) exprId context@(Context (scope, variableStack)) parent predecessors =
  let indexVariable = "index" ++ show exprId
      entitiesScope = scope ++ ".entities" ++ show exprId
      entityScope = entitiesScope ++ "[" ++ indexVariable ++ "]"
      entityValue = entityScope ++ ".value"
      updateCallback = scope ++ ".updateCallback" ++ show exprId
      (internalEntitiesVariable, sourceValueDependencies) = rightHandSideToJs variableStack sourceValue
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
          Ln (predecessorOf ++ " = (" ++ indexVariable ++ ") => {"),
          Ind
            [ Ln ("if (" ++ indexVariable ++ " < 0) {"),
              Ind
                [ Ln ("return " ++ predecessorChain predecessors)
                ],
              Ln ("} else if (" ++ entityScope ++ ".length === 0) {"),
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
          Ln ("let " ++ indexVariable ++ " = 0;"),
          Ln ("for (const " ++ entityVariable ++ " of " ++ internalEntitiesVariable ++ ") {"),
          Ind
            [ Ln (entitiesScope ++ ".push({value : " ++ entityVariable ++ "})"),
              Ln (createEntityCallback ++ "(" ++ indexVariable ++ ");"),
              Ln (indexVariable ++ "++;")
            ],
          Ln "}",
          Ln (updateCallback ++ "= () => {"),
          Ind
            [ Ln ("let " ++ indexVariable ++ " = 0;"),
              Ln ("for (const " ++ entityVariable ++ " of " ++ internalEntitiesVariable ++ ") {"),
              Ind
                [ Ln ("if (" ++ indexVariable ++ " < " ++ entitiesScope ++ ".length) {"),
                  Ind
                    ( Ln (entityValue ++ " = " ++ entityVariable) :
                      concatMap snd entityUpdateCallback
                    ),
                  Ln "} else {",
                  Ind
                    [ Ln (entitiesScope ++ ".push({value : " ++ entityVariable ++ "})"),
                      Ln (createEntityCallback ++ "(" ++ indexVariable ++ ")")
                    ],
                  Ln "}",
                  Ln (indexVariable ++ "++;")
                ],
              Ln "}",
              Ln ("let newCount = " ++ indexVariable ++ ";"),
              Ln ("for (let " ++ indexVariable ++ " = " ++ entitiesScope ++ ".length - 1; " ++ indexVariable ++ " >= newCount; " ++ indexVariable ++ "--) {"),
              Ind
                ( positiveRemoveCallbacks
                    ++ [Ln (entitiesScope ++ ".pop()")]
                ),
              Ln "}"
            ],
          Ln "}"
        ]
          ++ successorContent,
        exprId''',
        predecessors',
        UpdateCallbacks
          ( [(dependency, [Ln (updateCallback ++ "()")]) | dependency <- sourceValueDependencies] ++ restUpdateCallbacks'
          ),
        RemoveCallbacks []
      )
compileView ((Condition conditionValue positiveChildren negativeChildren) : ns) exprId context@(Context (scope, variableStack)) parent predecessors =
  let conditionVariable = scope ++ ".condition" ++ show exprId
      (positiveChildrenContent, exprId', positiveSuccessor, UpdateCallbacks positiveChildrenUpdateCallbacks, RemoveCallbacks positiveRemoveCallbacks) = compileView positiveChildren (exprId + 1) context parent predecessors
      (negativeChildrenContent, exprId'', negativeSuccessor, UpdateCallbacks negativeChildrenUpdateCallbacks, RemoveCallbacks negativeRemoveCallbacks) = compileView negativeChildren (exprId' + 1) context parent predecessors
      successor = "(" ++ conditionVariable ++ " ? " ++ predecessorChain positiveSuccessor ++ " : " ++ predecessorChain negativeSuccessor ++ ")"
      (internalConditionValue, conditionValueDependencies) = rightHandSideToJs variableStack conditionValue
      createPositiveCallback = scope ++ ".createPositive" ++ show exprId
      createNegativeCallback = scope ++ ".createNegative" ++ show exprId
      removeCallback = scope ++ ".removeCallback" ++ show exprId
      createCallback = "createCondition" ++ show exprId
      updateCallback = scope ++ ".updateCondition" ++ show exprId
      (successorContent, exprId''', predecessors', UpdateCallbacks successorUpdateCallbacks, RemoveCallbacks successorRemoveCallbacks) = compileView ns (exprId'' + 1) context parent (Predecessor successor : predecessors)
   in ( [ Ln (createPositiveCallback ++ " = () => {"),
          Ind positiveChildrenContent,
          Ln "}",
          Ln (createNegativeCallback ++ " = ()=> {"),
          Ind negativeChildrenContent,
          Ln "}",
          Ln ("const " ++ createCallback ++ " = () => {"),
          Ind
            [ Ln (conditionVariable ++ " = " ++ internalConditionValue ++ ";"),
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
              Ln (conditionVariable ++ " = " ++ internalConditionValue ++ ";"),
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
        exprId''',
        predecessors',
        UpdateCallbacks
          ( -- [ (internalVariableName, [Ln (updateCallback ++ "();")])
            -- ]
            [(dependency, [Ln (updateCallback ++ "()")]) | dependency <- conditionValueDependencies]
              -- TODO move to inline code section, instead of in callback section
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

rightHandSideToJs :: VariableStack -> RightHandSide -> (String, [String])
rightHandSideToJs variableStack (Variable variableParts) =
  let variableName = unsafeVariable (publicVariableToInternal variableStack variableParts)
   in (variableName, [variableName])
rightHandSideToJs variableStack (MixedTextValue []) = ("", [])
rightHandSideToJs variableStack (MixedTextValue ((StaticText staticText) : restMixedTextValues))
  | null restMixedTextValues = ("\"" ++ staticText ++ "\"", [])
  | otherwise = ("\"" ++ staticText ++ "\" ++ " ++ restValue, restDependencies)
  where
    (restValue, restDependencies) = rightHandSideToJs variableStack (MixedTextValue restMixedTextValues)
rightHandSideToJs variableStack (MixedTextValue ((DynamicText rightHandSide) : restMixedTextValues))
  | null restMixedTextValues = (currentValue, currentDependencies)
  | otherwise = (currentValue ++ restValue, currentDependencies ++ restDependencies)
  where
    (currentValue, currentDependencies) = rightHandSideToJs variableStack rightHandSide
    (restValue, restDependencies) = rightHandSideToJs variableStack (MixedTextValue restMixedTextValues)

-- TODO: a compileerror should be thrown instead
unsafeVariable :: Maybe String -> String
unsafeVariable (Just variable) = variable

type Child = String

appendChild :: Parent -> [Predecessor] -> Child -> String
appendChild parent [] child = parent ++ ".prepend(" ++ child ++ ");"
appendChild _ ((Predecessor predecessor) : ps) child = predecessor ++ ".after(" ++ child ++ ");"

predecessorChain :: [Predecessor] -> String
predecessorChain (Predecessor predecessor : ps) = predecessor
predecessorChain (MaybePredecessor predecessor : ps) = predecessor ++ " || " ++ predecessorChain ps
predecessorChain [] = "null"