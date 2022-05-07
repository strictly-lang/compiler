{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

module Emitter.Kinds.Expression where

import Control.Applicative ((<|>))
import Control.Monad.State.Lazy (get)
import Data.Foldable (find)
import Data.List (intercalate, isPrefixOf)
import Emitter.Types
import Emitter.Util (getFreshExprId, nameToVariable, slashToCamelCase, slashToDash, variableToString)
import Parser.Kinds.LeftHandSide (leftHandSideVariableParser)
import Types

---------------------
-- Boolean Handler --
---------------------
booleanHandler :: TypeHandler
booleanHandler = booleanHandlerByType

booleanHandlerByType :: TypeHandler
booleanHandlerByType stack typeDefinition@(Just (TypeAlgebraicDataType "String" [])) (Left ((selfDependency, code))) =
  Just
    ( StackHandler
        { runPrimitive =
            do
              return ([selfDependency], code),
          runFunctionApplication = \_ -> error "no function application implemented",
          runProperty = \_ -> error "no property access implemented",
          runResolvedType = typeDefinition
        }
    )
booleanHandlerByType _ _ _ = Nothing

--------------------
-- String Handler --
--------------------

stringHandler :: TypeHandler
stringHandler stack typeDefinition stackParameter = stringHandlerByLiteral stack typeDefinition stackParameter <|> stringHandlerByType stack typeDefinition stackParameter

stringHandlerByLiteral :: TypeHandler
stringHandlerByLiteral stack typeDefinition (Right (RightHandSideString strings)) =
  Just
    ( StackHandler
        { runPrimitive =
            do
              result <-
                mapM
                  ( \case
                      RightHandSideStringStatic static -> do
                        return ([], [Ln static])
                      RightHandSideStringDynamic untypedExpression -> do
                        (typedExpression) <- toTypedExpression stack typeDefinition untypedExpression
                        (dependencies, result) <- runPrimitive typedExpression

                        return (dependencies, Ln "${" : result ++ [Ln "}"])
                  )
                  strings
              return (concatMap fst result, Ln "`" : (concatMap snd result) ++ [Ln "`"]),
          runFunctionApplication = \_ -> error "no function application implemented",
          runProperty = \_ -> error "no property access implemented",
          runResolvedType = typeDefinition
        }
    )
stringHandlerByLiteral _ _ _ = Nothing

stringHandlerByType :: TypeHandler
stringHandlerByType stack typeDefinition@(Just (TypeAlgebraicDataType "String" [])) (Left ((selfDependency, code))) =
  Just
    ( StackHandler
        { runPrimitive =
            do
              return ([selfDependency], code),
          runFunctionApplication = \_ -> error "no function application implemented",
          runProperty = \_ -> error "no property access implemented",
          runResolvedType = typeDefinition
        }
    )
stringHandlerByType _ _ _ = Nothing

--------------------
-- Record Handler --
--------------------

recordHandler :: TypeHandler
recordHandler stack typeDefinition@(Just (TypeRecord properties)) untypedExpression =
  Just
    StackHandler
      { runPrimitive =
          do
            case untypedExpression of
              Left ((selfDependency, code)) -> do
                return ([selfDependency], code)
              Right _ ->
                error (show untypedExpression),
        runFunctionApplication = \_ -> error "no function application implemented",
        runProperty = \propertyName -> do
          case find (\(propertyName', typeDefinition) -> propertyName == propertyName') properties of
            Just (_, propertyType) ->
              case untypedExpression of
                Left (dependency, code) -> do
                  let property = (dependency ++ [DotNotation propertyName], code ++ [Ln ("." ++ propertyName)])
                  return ((findType stack typeDefinition (Left property)))
                Right _ ->
                  error (show untypedExpression)
            Nothing -> error ("could not find " ++ propertyName),
        runResolvedType = typeDefinition
      }
recordHandler _ _ _ = Nothing

----------------------
-- Function Handler --
----------------------
functionHandler :: TypeHandler
functionHandler stack typeDefinition stackParameter = functionHandlerByLiteral stack typeDefinition stackParameter <|> functionHandlerByType stack typeDefinition stackParameter

functionHandlerByLiteral :: TypeHandler
functionHandlerByLiteral stack typeDefinition@(Just (TypeFunction typeParameters typeReturn)) (Right (RightHandSideFunctionDefinition parameters body)) =
  Just
    StackHandler
      { runPrimitive = do
          result <- code stack body
          return
            ( [],
              ( [ Ln "((",
                  Ln ") => {",
                  Ind result,
                  Ln "})"
                ]
              )
            ),
        runFunctionApplication = \parameters -> do
          error "function application not yet implemented in literal",
        runProperty = \_ -> error "no property access implemented",
        runResolvedType = typeDefinition
      }
functionHandlerByLiteral _ _ _ = Nothing

functionHandlerByType :: TypeHandler
functionHandlerByType stack typeDefinition@(Just (TypeFunction typeParameters typeReturn)) (Left ((selfDependency, code))) =
  Just
    StackHandler
      { runPrimitive = do return ([selfDependency], code),
        runFunctionApplication = \parameters -> error "function application not yet implemented in reference",
        runProperty = \_ -> error "no property access implemented",
        runResolvedType = typeDefinition
      }
functionHandlerByType _ _ _ = Nothing

code :: Stack -> [Statement] -> AppStateMonad [Code]
code stack [] = do return []
code stack ((UntypedExpression untypedExpression) : restStatements) = do
  typedExpression <- toTypedExpression stack Nothing untypedExpression
  (dependencies, result) <- runPrimitive typedExpression
  nextCode <- code stack restStatements
  return (result ++ nextCode)

-----------------------
-- Component Handler --
-----------------------

componentHandler :: TypeHandler
componentHandler stack typeDefinition@(Just (TypeFunction [typedProperties, _] (TypeAlgebraicDataType "View" []))) (Right (RightHandSideFunctionDefinition [leftHandSideProperties, leftHandSideAttributes] body)) =
  Just
    ( StackHandler
        { runPrimitive =
            do
              appState <- get
              exprId <- getFreshExprId
              let scope = [DotNotation "this"]
              let componentName' = componentName appState
              let unscopedMounted = nameToVariable "mounted" exprId
              let unscopedProperties = nameToVariable "properties" exprId
              let scopedMounted = scope ++ unscopedMounted
              let scopedProperties = scope ++ unscopedProperties
              -- let typedProperties = typedOrigin scopedProperties propertyTypes
              let stack' = addToVariableStack stack [(leftHandSideProperties, typedOrigin stack scopedProperties (Just typedProperties))]
              let TypeRecord propertyTypes = typedProperties
              view <- render stack' body scope (scope ++ [DotNotation "shadowRoot"]) []
              let dependencies = runViewUpdate view

              propertySetters <-
                ( mapM
                    ( \(propertyName, _) -> do
                        exprId <- getFreshExprId
                        let propertyValue = nameToVariable "propertyValue" exprId
                        let propertyPath = (scopedProperties ++ ([DotNotation propertyName]))
                        let dependency = filter ((isPrefixOf propertyPath) . fst) dependencies
                        return
                          [ Ln
                              ("set " ++ propertyName ++ "(" ++ variableToString propertyValue ++ ") {"),
                            Ind
                              ( [ Ln
                                    ( variableToString propertyPath
                                        ++ " = "
                                        ++ variableToString propertyValue
                                        ++ ";"
                                    ),
                                  Br,
                                  Ln ("if (" ++ variableToString scopedMounted ++ ") {"),
                                  Ind (concatMap snd dependency),
                                  Ln "}"
                                ]
                              ),
                            Ln "}",
                            Br
                          ]
                    )
                    propertyTypes
                  )

              return
                ( [],
                  [ Ln ("class " ++ slashToCamelCase componentName' ++ " extends HTMLElement {"),
                    Ind
                      ( [ Ln (variableToString unscopedMounted ++ " = false;"),
                          Br,
                          Ln (variableToString unscopedProperties ++ " = {};"),
                          Br,
                          Ln "connectedCallback() {",
                          Ind
                            ( [ Ln "this.attachShadow({mode: 'open'});",
                                Br,
                                Ln (variableToString scopedMounted ++ " = true;"),
                                Br
                              ]
                                ++ runViewCreate view
                            ),
                          Br,
                          Ln "}",
                          Br
                        ]
                          ++ concat propertySetters
                      ),
                    Ln "}",
                    Br,
                    Br,
                    Ln ("customElements.define(\"" ++ slashToDash componentName' ++ "\", " ++ slashToCamelCase componentName' ++ ");"),
                    Br
                  ]
                ),
          runFunctionApplication = \_ -> error "no function application implemented",
          runProperty = \_ -> error "no property access implemented",
          runResolvedType = typeDefinition
        }
    )
componentHandler _ _ _ = Nothing

prelude :: [StackEntry]
prelude =
  [ StackType booleanHandler,
    StackType stringHandler,
    StackType recordHandler,
    StackType componentHandler,
    StackType functionHandler
  ]

------------------------------
-- TypedExpression handling --
------------------------------

toTypedExpression :: Stack -> Maybe TypeDefinition -> UntypedExpression -> AppStateMonad StackHandler
toTypedExpression stack typeDefinition (firstExpression : restUntypedExpression) = do
  firstTypedExpression <- toTypedExpression' stack typeDefinition firstExpression
  nestedTypedExpression stack firstTypedExpression restUntypedExpression

toTypedExpression' :: Stack -> Maybe TypeDefinition -> UntypedExpression' -> AppStateMonad StackHandler
toTypedExpression' stack typeDefinition ((RightHandSideVariable variableName)) = do
  case ( find
           ( \case
               (StackValue (variableName', _)) -> variableName' == variableName
               _ -> False
           )
           stack
       ) of
    Just (StackValue (_, typedExpression)) -> do return typedExpression
    Nothing -> error ("Could not find variable" ++ variableName)
toTypedExpression' stack typeDefinition (untypedExpression) = do
  return ((findType stack typeDefinition (Right untypedExpression)))

nestedTypedExpression :: Stack -> StackHandler -> UntypedExpression -> AppStateMonad StackHandler
nestedTypedExpression stack typedExpression [] = do return typedExpression
nestedTypedExpression stack (typedExpression) ((RightHandSideVariable propertyName) : restUntypedExpression) = do
  typedProperty <- runProperty typedExpression propertyName
  (nestedTypedExpression stack (typedProperty)) restUntypedExpression
nestedTypedExpression stack (typedExpression) ((RightHandSideFunctionCall (parameters)) : restUntypedExpression) = do
  typedParameters <- mapM (toTypedExpression stack Nothing) parameters
  typedProperty <- runFunctionApplication typedExpression typedParameters
  (nestedTypedExpression stack (typedProperty)) restUntypedExpression
nestedTypedExpression stack typedExpression untypedExpression = do
  error ("cant nest " ++ show untypedExpression)

findType :: Stack -> Maybe TypeDefinition -> StackParameter -> StackHandler
findType stack = findType' (stack, stack)

findType' :: (Stack, Stack) -> Maybe TypeDefinition -> StackParameter -> StackHandler
findType' ([], _) typeDefinition stackParameter = error ("no corresponding type found " ++ show typeDefinition)
findType' ((StackType stackEntry) : nextStack, allStack) typeDefinition stackParameter =
  case stackEntry allStack typeDefinition stackParameter of
    Just result -> result
    Nothing -> findType' (nextStack, allStack) typeDefinition stackParameter
findType' (_ : nextStack, allStack) typeDefinition stackParameter = findType' (nextStack, allStack) typeDefinition stackParameter

addToVariableStack :: Stack -> [(LeftHandSide, StackHandler)] -> Stack
addToVariableStack variableStack [] = variableStack
addToVariableStack variableStack ((LeftHandSideHole, _) : restNewVariables) = addToVariableStack variableStack restNewVariables
addToVariableStack variableStack ((LeftHandSideVariable name, typedExpression) : restNewVariables) = (StackValue (name, typedExpression)) : addToVariableStack variableStack restNewVariables

-- view

render :: Stack -> [Statement] -> [Variable] -> Parent -> [Sibling] -> AppStateMonad ViewResult
render variableStack [] scope parent siblings = do
  return
    ViewResult {runViewCreate = [], runViewUpdate = [], runViewUnmount = [], runViewDelete = [], runSiblings = siblings}
render variableStack ((UntypedExpression [RightHandSideHost elementName (properties, _) children]) : restUntypedBody) scope parent siblings = do
  exprId <- getFreshExprId
  let hostElement = scope ++ nameToVariable "element" exprId
  childrenResult <- render variableStack children scope hostElement []
  siblingResult <- render variableStack restUntypedBody scope parent (SiblingAlways hostElement : siblings)
  propertiesResult <-
    mapM
      ( \(propertyName, RecordExpression _ propertyExpression) ->
          let eventPrefix = "on"
           in if (eventPrefix `isPrefixOf` propertyName)
                then do
                  (typedPropertyExpression) <- toTypedExpression variableStack (Just (TypeFunction [TypeRecord []] (TypeAlgebraicDataType "Void" ([])))) propertyExpression
                  (dependencies, code) <- runPrimitive typedPropertyExpression
                  return
                    ( [],
                      ( Ln (variableToString hostElement ++ ".addEventListener(\"" ++ drop (length eventPrefix) propertyName ++ "\", ") :
                        code
                          ++ [ Ln ");",
                               Br
                             ]
                      )
                    )
                else do
                  (typedPropertyExpression) <- toTypedExpression variableStack (Just ((TypeAlgebraicDataType "String" ([])))) propertyExpression
                  (dependencies, code) <- runPrimitive typedPropertyExpression
                  return
                    ( dependencies,
                      ( Ln (variableToString hostElement ++ ".setAttribute(\"" ++ propertyName ++ "\", ") :
                        code
                          ++ [ Ln ");",
                               Br
                             ]
                      )
                    )
      )
      properties

  return
    ( ViewResult
        { runViewCreate =
            [ Ln (variableToString hostElement ++ " = document.createElement(\"" ++ elementName ++ "\");"),
              Br
            ]
              ++ concatMap snd propertiesResult
              ++ appendElement parent siblings hostElement
              ++ runViewCreate childrenResult
              ++ runViewCreate siblingResult,
          runViewUpdate =
            [(dependency, code) | (dependencies, code) <- propertiesResult, dependency <- dependencies]
              ++ runViewUpdate childrenResult
              ++ runViewUpdate siblingResult,
          runViewUnmount =
            runViewUnmount childrenResult
              ++ runViewUnmount siblingResult,
          runViewDelete =
            Ln (variableToString hostElement ++ ".remove();") :
            Br :
            runViewDelete siblingResult,
          runSiblings = runSiblings siblingResult
        }
    )
render variableStack ((UntypedExpression [RightHandSideCondition conditionExpression thenStatements elseStatements]) : restUntypedBody) scope parent siblings = do
  exprId <- getFreshExprId
  typedCondition <- toTypedExpression variableStack (Just (TypeAlgebraicDataType "Boolean" [])) conditionExpression
  (dependencies, typedConditionResult) <- runPrimitive typedCondition

  thenResult <- render variableStack thenStatements scope parent siblings
  elseResult <- render variableStack elseStatements scope parent siblings

  let thenCallback = scope ++ nameToVariable "thenCallback" exprId
  let elseCallback = scope ++ nameToVariable "elseCallback" exprId
  let conditionStorage = scope ++ nameToVariable "conditionStorage" exprId
  let siblings' =
        SiblingCondition [(Ln (variableToString conditionStorage))] (runSiblings thenResult) (runSiblings elseResult) :
        siblings

  siblingResult <-
    render
      variableStack
      restUntypedBody
      scope
      parent
      siblings'

  return
    ( ViewResult
        { runViewCreate =
            [ Ln (variableToString thenCallback ++ " = () => {"),
              Ind (runViewCreate thenResult),
              Ln "};",
              Br,
              Ln (variableToString elseCallback ++ " = () => {"),
              Ind (runViewCreate elseResult),
              Ln "};",
              Br,
              Ln (variableToString conditionStorage ++ " = ")
            ]
              ++ typedConditionResult
              ++ [ Ln ";",
                   Br,
                   Ln ("if (" ++ variableToString conditionStorage ++ ") {"),
                   Ind
                     [ Ln (variableToString thenCallback ++ "();")
                     ],
                   Ln "} else {",
                   Ind
                     [ Ln (variableToString elseCallback ++ "();")
                     ],
                   Ln "}",
                   Br
                 ]
              ++ runViewCreate siblingResult,
          runViewUpdate =
            [ ( dependency,
                Ln
                  ( "if ("
                      ++ variableToString conditionStorage
                      ++ " !== "
                  ) :
                typedConditionResult
                  ++ [ Ln ") {",
                       Ind
                         [ Ln (variableToString conditionStorage ++ " = !" ++ variableToString conditionStorage ++ ";"),
                           Br,
                           Ln ("if (" ++ variableToString conditionStorage ++ ") {"),
                           Ind
                             ( runViewUnmount
                                 elseResult
                                 ++ runViewDelete
                                   elseResult
                                 ++ [ Ln (variableToString thenCallback ++ "();")
                                    ]
                             ),
                           Ln "} else {",
                           Ind
                             ( runViewUnmount
                                 thenResult
                                 ++ runViewDelete
                                   thenResult
                                 ++ [ Ln (variableToString elseCallback ++ "();")
                                    ]
                             ),
                           Ln "}",
                           Br
                         ],
                       Ln "}",
                       Br
                     ]
              )
              | dependency <- dependencies
            ]
              ++ [ (dependency, [Ln ("if (" ++ variableToString conditionStorage ++ ") {"), Ind update, Ln "}", Br]) | (dependency, update) <- runViewUpdate thenResult
                 ]
              ++ [ (dependency, [Ln ("if (" ++ variableToString conditionStorage ++ " === false) {"), Ind update, Ln "}", Br]) | (dependency, update) <- runViewUpdate elseResult
                 ]
              ++ runViewUpdate siblingResult,
          runViewUnmount =
            [ Ln ("if (" ++ variableToString conditionStorage ++ ") {"),
              Ind (runViewUnmount thenResult),
              Ln "} else {",
              Ind (runViewUnmount elseResult),
              Ln "}",
              Br
            ]
              ++ runViewUnmount siblingResult,
          runViewDelete =
            [ Ln ("if (" ++ variableToString conditionStorage ++ ") {"),
              Ind (runViewDelete thenResult),
              Ln "} else {",
              Ind (runViewDelete elseResult),
              Ln "}",
              Br
            ]
              ++ runViewDelete siblingResult,
          runSiblings = siblings'
        }
    )
render variableStack ((UntypedExpression untypedExpression) : restUntypedBody) scope parent siblings = do
  typedResult <- toTypedExpression variableStack (Just (TypeAlgebraicDataType "String" [])) untypedExpression
  exprId <- getFreshExprId
  let textElement = scope ++ nameToVariable "text" exprId
  (dependencies, textContent) <- runPrimitive typedResult
  siblingResult <- render variableStack restUntypedBody scope parent (SiblingAlways textElement : siblings)

  return
    ( ViewResult
        { runViewCreate =
            Ln (variableToString textElement ++ " = document.createTextNode(") :
            textContent
              ++ [ Ln ");",
                   Br
                 ]
              ++ appendElement parent siblings textElement
              ++ runViewCreate siblingResult,
          runViewUpdate =
            ( concatMap
                ( \dependency ->
                    [ ( dependency,
                        Ln (variableToString textElement ++ ".textContent = ") : textContent ++ ([Ln ";", Br])
                      )
                    ]
                )
                dependencies
            )
              ++ runViewUpdate siblingResult,
          runViewUnmount = runViewUnmount siblingResult,
          runViewDelete = Ln (variableToString textElement ++ ".remove();") : Br : runViewDelete siblingResult,
          runSiblings = runSiblings siblingResult
        }
    )
render variableStack untypedBody scope parent siblings = error "mep"

data Predecessor = PredecessorNone | PredecessorAlways [Code] | PredecessorMaybe [Code] [Code]

appendElement :: Parent -> [Sibling] -> [Variable] -> [Code]
appendElement parent siblings target =
  let result = appendElement' siblings
      parent' = variableToString parent
      target' = variableToString target
      siblingsAfter sibling = sibling ++ [Ln (".after(" ++ target' ++ ");"), Br]
      siblingsNone = [Ln (parent' ++ ".append(" ++ target' ++ ");"), Br]
   in case result of
        PredecessorAlways predecessor ->
          siblingsAfter predecessor
        PredecessorMaybe condition predecessor ->
          ( Ln "if (" :
            condition
              ++ [ Ln ") {",
                   Ind (siblingsAfter predecessor),
                   Ln "} else {",
                   Ind siblingsNone,
                   Ln "}",
                   Br
                 ]
          )
        PredecessorNone ->
          siblingsNone

appendElement' :: [Sibling] -> Predecessor
appendElement' [] = PredecessorNone
appendElement' ((SiblingAlways sibling) : restSiblings) = PredecessorAlways [Ln (variableToString sibling)]
appendElement' ((SiblingCondition condition thenSiblings elseSiblings) : restSiblings) =
  let thenResult = appendElement' thenSiblings
      elseResult = appendElement' elseSiblings
   in case thenResult of
        PredecessorAlways thenResult' ->
          case elseResult of
            PredecessorAlways elseResult' ->
              PredecessorAlways
                ( Ln "(" :
                  condition ++ [Ln " ? "]
                    ++ thenResult'
                    ++ [Ln " : "]
                    ++ elseResult'
                    ++ [Ln ")"]
                )
            PredecessorMaybe elseCondition elseResult' ->
              PredecessorMaybe
                (Ln "(" : condition ++ [Ln " || "] ++ elseCondition ++ [Ln ")"])
                (Ln "(" : condition ++ [Ln " ? "] ++ thenResult' ++ Ln " : " : elseResult' ++ [Ln ")"])
            PredecessorNone ->
              PredecessorMaybe condition thenResult'
        PredecessorMaybe thenCondition thenResult' ->
          case elseResult of
            PredecessorAlways elseResult' ->
              PredecessorMaybe
                ( Ln "(" :
                  condition
                    ++ [Ln " === false || "]
                    ++ thenCondition
                    ++ [Ln ")"]
                )
                ( Ln "(" :
                  condition ++ [Ln " ? "] ++ thenResult' ++ [Ln " ? "] ++ elseResult' ++ [Ln ")"]
                )
            PredecessorMaybe elseCondition elseResult' ->
              PredecessorMaybe
                ( Ln "((" :
                  condition ++ [Ln " && "] ++ thenCondition
                    ++ [Ln ") || ("]
                    ++ condition
                    ++ [Ln " === false && "]
                    ++ elseCondition
                    ++ [Ln "))"]
                )
                ( Ln "(" :
                  condition ++ [Ln " ? "] ++ thenResult' ++ [Ln " ? "] ++ elseResult' ++ [Ln ")"]
                )
            PredecessorNone ->
              PredecessorMaybe
                (Ln "(" : condition ++ [Ln " && "] ++ thenCondition ++ [Ln ")"])
                thenResult'
        PredecessorNone ->
          case elseResult of
            PredecessorAlways elseResult' ->
              PredecessorMaybe
                (Ln "(" : condition ++ [Ln " === false)"])
                elseResult'
            PredecessorMaybe elseCondition elseResult' ->
              PredecessorMaybe
                (Ln "(" : condition ++ [Ln " === false && "] ++ elseCondition ++ [Ln ")"])
                elseResult'
            PredecessorNone ->
              PredecessorNone

typedOrigin :: Stack -> [Variable] -> Maybe TypeDefinition -> StackHandler
typedOrigin stack variablePath typeDefinition = (findType stack typeDefinition (Left (variablePath, [Ln (variableToString variablePath)])))