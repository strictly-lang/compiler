{-# LANGUAGE LambdaCase #-}

module Emitter.Kinds.Expression where

import Control.Applicative ((<|>))
import Control.Monad.State.Lazy (get)
import Data.Foldable (find)
import Data.List (groupBy, intercalate, intersperse, isPrefixOf)
import Data.Maybe (isJust)
import Emitter.Types
import Emitter.Util (getFreshExprId, groupProperties, nameToVariable, slashToCamelCase, slashToDash, variableToString)
import Parser.Kinds.LeftHandSide (leftHandSideVariableParser)
import Types

---------------------
-- Operator Handler --
---------------------
operatorHandler :: TypeHandler
operatorHandler = operatorEqualOperator

operatorEqualOperator :: TypeHandler
operatorEqualOperator stack typeDefinition (Right [[RightHandSideOperator Equal firstExpression secondExpression]]) =
  Just
    ( do
        return
          StackHandler
            { runPrimitive =
                do
                  firstExpressionHandler <- toTypedExpression stack TypeUnknown [firstExpression]
                  firstExpressionPrimitive <- runPrimitive firstExpressionHandler
                  secondExpressionHandler <- toTypedExpression stack TypeUnknown [secondExpression]
                  secondExpressionPrimitive <- runPrimitive secondExpressionHandler
                  return (fst firstExpressionPrimitive ++ fst secondExpressionPrimitive, snd firstExpressionPrimitive ++ [Ln " == "] ++ snd secondExpressionPrimitive),
              runFunctionApplication = \_ -> error "no function application implemented in boolean",
              runProperty = \_ -> error "no property access implemented",
              runViewStream = \_ -> error "no streaming",
              runResolvedType = typeDefinition,
              runPatternMatching = \_ -> error "no pattern access implemented"
            }
    )
operatorEqualOperator _ _ _ = Nothing

---------------------
-- Number Handler --
---------------------
numberHandler :: TypeHandler
numberHandler stack typeDefinition stackParameter = numberHandlerByLiteral stack typeDefinition stackParameter <|> numberHandlerByType stack typeDefinition stackParameter

numberHandlerByLiteral :: TypeHandler
numberHandlerByLiteral stack typeDefinition (Right [[RightHandSideNumber int]]) =
  Just
    ( do
        return
          StackHandler
            { runPrimitive =
                do
                  return ([], [Ln (show int)]),
              runFunctionApplication = \_ -> error "no function application implemented in number",
              runProperty = \_ -> error "no property access implemented",
              runViewStream = \_ -> error "no streaming",
              runResolvedType = typeDefinition `infers` TypeAlgebraicDataType "Number" [],
              runPatternMatching = \_ -> error "no pattern access implemented"
            }
    )
numberHandlerByLiteral _ _ _ = Nothing

numberHandlerByType :: TypeHandler
numberHandlerByType stack typeDefinition@((TypeAlgebraicDataType "Number" [])) (Left (selfDependency, code)) =
  Just
    ( do
        return
          StackHandler
            { runPrimitive =
                do
                  return (selfDependency, code),
              runFunctionApplication = \_ -> error "no function application implemented in number",
              runProperty = \_ -> error "no property access implemented",
              runViewStream = \_ -> error "no streaming",
              runResolvedType = typeDefinition,
              runPatternMatching = \_ -> error "no pattern access implemented"
            }
    )
numberHandlerByType _ _ _ = Nothing

---------------------
-- Boolean Handler --
---------------------
booleanHandler :: TypeHandler
booleanHandler = booleanHandlerByType

booleanHandlerByType :: TypeHandler
booleanHandlerByType stack typeDefinition@((TypeAlgebraicDataType "Boolean" [])) (Left (selfDependency, code)) =
  Just
    ( do
        return
          StackHandler
            { runPrimitive =
                do
                  return (selfDependency, code),
              runFunctionApplication = \_ -> error "no function application implemented in boolean",
              runProperty = \_ -> error "no property access implemented",
              runViewStream = \_ -> error "no streaming",
              runResolvedType = typeDefinition,
              runPatternMatching = \_ -> error "no pattern access implemented"
            }
    )
booleanHandlerByType _ _ _ = Nothing

--------------------
-- String Handler --
--------------------

stringHandler :: TypeHandler
stringHandler stack typeDefinition stackParameter = stringHandlerByLiteral stack typeDefinition stackParameter <|> stringHandlerByType stack typeDefinition stackParameter

stringHandlerByLiteral :: TypeHandler
stringHandlerByLiteral stack typeDefinition (Right [[RightHandSideString strings]]) =
  Just
    ( do
        return
          ( StackHandler
              { runPrimitive =
                  do
                    result <-
                      mapM
                        ( \case
                            RightHandSideStringStatic static -> do
                              return ([], [Ln static])
                            RightHandSideStringDynamic untypedExpression -> do
                              typedExpression <- toTypedExpression stack typeDefinition [untypedExpression]
                              (dependencies, result) <- runPrimitive typedExpression

                              return (dependencies, Ln "${" : result ++ [Ln "}"])
                        )
                        strings
                    return (concatMap fst result, Ln "`" : concatMap snd result ++ [Ln "`"]),
                runFunctionApplication = \_ -> error "no function application implemented in stringliteral",
                runProperty = \_ -> error "no property access implemented",
                runViewStream = \_ -> error "no streaming",
                runResolvedType = typeDefinition `infers` TypeAlgebraicDataType "String" [],
                runPatternMatching = \_ -> error "no pattern access implemented"
              }
          )
    )
stringHandlerByLiteral _ _ _ = Nothing

stringHandlerByType :: TypeHandler
stringHandlerByType stack typeDefinition@((TypeAlgebraicDataType "String" [])) (Left (selfDependency, code)) =
  Just
    ( do
        return
          ( StackHandler
              { runPrimitive =
                  do
                    return (selfDependency, code),
                runFunctionApplication = \_ -> error "no function application implemented in stringreference",
                runProperty = \_ -> error "no property access implemented",
                runViewStream = \_ -> error "no streaming",
                runResolvedType = typeDefinition,
                runPatternMatching = \_ -> error "no pattern access implemented"
              }
          )
    )
stringHandlerByType _ _ _ = Nothing

--------------------
-- Record Handler --
--------------------

recordHandler :: TypeHandler
recordHandler stack typeDefinition@((TypeRecord properties)) (Left ([selfDependency], code)) =
  let stackHandler =
        StackHandler
          { runPrimitive =
              do
                return ([selfDependency], code),
            runFunctionApplication = \_ -> error "no function application implemented in record",
            runProperty = \propertyName -> do
              case find (\(propertyName', typeDefinition) -> propertyName == propertyName') properties of
                Just (_, propertyType) -> do
                  let property = ([selfDependency ++ [DotNotation propertyName]], code ++ [Ln ("." ++ propertyName)])
                  findType stack propertyType (Left property)
                Nothing -> error ("could not find " ++ propertyName ++ show typeDefinition),
            runViewStream = \_ -> error "no streaming",
            runResolvedType = typeDefinition,
            runPatternMatching = \_ -> error "no pattern access implemented"
          }
   in Just (return stackHandler)
recordHandler _ _ _ = Nothing

---------------------
-- List Handler --
---------------------
listHandler :: TypeHandler
listHandler stack typeDefinition stackParameter = listHandlerByLiteral stack typeDefinition stackParameter <|> listHandlerByType stack typeDefinition stackParameter

listHandlerByLiteral :: TypeHandler
listHandlerByLiteral stack typeDefinition (Right [[RightHandSideList listEntities []]]) =
  Just
    ( do
        let listEntityType =
              ( case typeDefinition of
                  ((TypeList listEntityType)) -> listEntityType
                  _ -> TypeUnknown
              )
        entityStackHandler <- mapM (\listEntity -> toTypedExpression stack listEntityType [listEntity]) listEntities
        primitives <- mapM runPrimitive entityStackHandler

        let stackHandler =
              ( StackHandler
                  { runPrimitive =
                      do
                        return (concatMap fst primitives, Ln "[" : intercalate [Ln ", "] (map snd primitives) ++ [Ln "]"]),
                    runFunctionApplication = \_ -> error "no function application implemented in list",
                    runProperty = listHandlerRunProperty stackHandler stack,
                    runViewStream = listHandlerRunViewStream stackHandler stack,
                    runResolvedType = typeDefinition `infers` TypeList listEntityType,
                    runPatternMatching = \_ -> error "no pattern access implemented"
                  }
              )
        return stackHandler
    )
listHandlerByLiteral _ _ _ = Nothing

listHandlerByType :: TypeHandler
listHandlerByType stack typeDefinition@((TypeList listEntityType)) (Left (selfDependency, code)) =
  Just
    ( do
        let stackHandler =
              StackHandler
                { runPrimitive =
                    do
                      return (selfDependency, code),
                  runFunctionApplication = \_ -> error "no function application implemented in list",
                  runProperty = listHandlerRunProperty stackHandler stack,
                  runViewStream = listHandlerRunViewStream stackHandler stack,
                  runResolvedType = typeDefinition,
                  runPatternMatching = \_ -> error "no pattern access implemented"
                }
        return stackHandler
    )
listHandlerByType _ _ _ = Nothing

listHandlerRunProperty :: StackHandler -> Stack -> String -> AppStateMonad StackHandler
listHandlerRunProperty listStackHandler stack "length" = do
  (dependencies, code) <- runPrimitive listStackHandler
  findType stack (TypeAlgebraicDataType "Number" []) (Left (dependencies, code ++ [Ln ".length"]))

listHandlerRunViewStream :: StackHandler -> Stack -> [Variable] -> Parent -> [Sibling] -> LeftHandSide -> [Statement] -> AppStateMonad ViewResult
listHandlerRunViewStream stackHandler stack scope parent siblings leftHandSide body = do
  exprId <- getFreshExprId
  let scopeContainer = scope ++ nameToVariable "scopeContainer" exprId
  let index = nameToVariable "index" exprId
  let entityScope = scopeContainer ++ [BracketNotation (variableToString index)]
  let eachCallback = scope ++ nameToVariable "eachCallback" exprId

  let iterable = scope ++ nameToVariable "iterable" exprId
  let entityType = case runResolvedType stackHandler of
        TypeList entityType -> entityType
        _ -> TypeUnknown

  (dependencies, iterableCode) <- runPrimitive stackHandler
  entityStackHandler <- typedOrigin stack (iterable ++ [BracketNotation (variableToString index)]) entityType
  stack' <- addToVariableStack stack [(leftHandSide, entityStackHandler)]
  entityViewResult <- render stack' body entityScope parent siblings

  return
    ( ViewResult
        { runViewCreate =
            [ Ln (variableToString scopeContainer ++ " = {};"),
              Br,
              Ln (variableToString eachCallback ++ " = (" ++ variableToString index ++ ") => {"),
              Ind (runViewCreate entityViewResult),
              Ln "};",
              Br,
              Ln (variableToString iterable ++ " = ")
            ]
              ++ iterableCode
              ++ [ Br,
                   Ln ("for(let " ++ variableToString index ++ " = 0; " ++ variableToString index ++ " < " ++ variableToString iterable ++ ".length; " ++ variableToString index ++ " += 1) {"),
                   Ind
                     [ Ln (variableToString entityScope ++ " = {};"),
                       Br,
                       Ln (variableToString eachCallback ++ "(" ++ variableToString index ++ ");")
                     ],
                   Ln "}",
                   Br
                 ],
          runViewUpdate = [],
          runViewUnmount = [],
          runViewDelete = [],
          runSiblings = siblings
        }
    )

-------------------
-- Tuple Handler --
-------------------
tupleHandler :: TypeHandler
tupleHandler = tupleHandlerByType

tupleHandlerByType :: TypeHandler
tupleHandlerByType stack typeDefinition@(TypeTuple tupleTypes) (Left (selfDependency, code)) =
  Just
    ( do
        let stackHandler =
              StackHandler
                { runPrimitive =
                    do
                      return (selfDependency, code),
                  runFunctionApplication = \_ -> error "no function application implemented in list",
                  runProperty = listHandlerRunProperty stackHandler stack,
                  runViewStream = listHandlerRunViewStream stackHandler stack,
                  runResolvedType = typeDefinition,
                  runPatternMatching = \(LeftHandSideList leftHandSides) -> do
                    return []
                }
        return stackHandler
    )
tupleHandlerByType _ _ _ = Nothing

-------------------
-- Range Handler --
-------------------
rangeHandler :: TypeHandler
rangeHandler stack typeDefinition (Right [[RightHandSideRange from to]]) =
  Just
    ( do
        return
          ( StackHandler
              { runPrimitive =
                  do
                    return ([], []),
                runFunctionApplication = \_ -> error "no function application implemented in void",
                runProperty = \_ -> error "no property access implemented",
                runViewStream = \_ -> error "no streaming",
                runResolvedType = typeDefinition,
                runPatternMatching = \_ -> error "no pattern access implemented"
              }
          )
    )
rangeHandler _ _ _ = Nothing

----------------------
-- Function Handler --
----------------------
functionHandler :: TypeHandler
functionHandler stack typeDefinition stackParameter = functionHandlerByLiteral stack typeDefinition stackParameter <|> functionHandlerByType stack typeDefinition stackParameter

functionHandlerByLiteral :: TypeHandler
functionHandlerByLiteral stack typeDefinition@((TypeFunction typeParameters typeReturn)) (Right [[RightHandSideFunctionDefinition parameters body]]) =
  Just
    ( do
        return
          ( StackHandler
              { runPrimitive = do
                  parameterNames <-
                    mapM
                      ( const
                          (nameToVariable "param" <$> getFreshExprId)
                      )
                      typeParameters
                  parameterStackHandlers <- mapM (\(typeDefinition, parameterName) -> typedOrigin stack parameterName typeDefinition) (zip typeParameters parameterNames)
                  stack' <- addToVariableStack stack (zip parameters parameterStackHandlers)

                  result <- code stack' body
                  return
                    ( [],
                      Ln "((" :
                      intersperse (Ln ",") (map (Ln <$> variableToString) parameterNames)
                        ++ [ Ln ") => {",
                             Ind result,
                             Ln "})"
                           ]
                    ),
                runFunctionApplication = \parameters -> do
                  error "function application not yet implemented in literal",
                runProperty = \_ -> error "no property access implemented",
                runViewStream = \_ -> error "no streaming",
                runResolvedType = typeDefinition `infers` TypeFunction [] TypeUnknown,
                runPatternMatching = \_ -> error "no pattern access implemented"
              }
          )
    )
functionHandlerByLiteral _ _ _ = Nothing

functionHandlerByType :: TypeHandler
functionHandlerByType stack typeDefinition@((TypeFunction typeParameters typeReturn)) (Left (selfDependency, code)) =
  let stackHandler =
        StackHandler
          { runPrimitive = do return (selfDependency, code),
            runFunctionApplication = \parameters -> do
              (primitive, code) <- runPrimitive stackHandler
              parameterPrimitives <- mapM runPrimitive parameters

              findType
                stack
                typeReturn
                ( Left
                    ( [],
                      code
                        ++ [ Ln "("
                           ]
                        ++ intercalate [Ln ","] (map snd parameterPrimitives)
                        ++ [ Ln ")"
                           ]
                    )
                ),
            runProperty = \_ -> error "no property access implemented",
            runViewStream = \_ -> error "no streaming",
            runResolvedType = typeDefinition,
            runPatternMatching = \_ -> error "no pattern access implemented"
          }
   in Just (return stackHandler)
functionHandlerByType _ _ _ = Nothing

code :: Stack -> [Statement] -> AppStateMonad [Code]
code stack [] = do return []
code stack ((UntypedExpression untypedExpression) : restStatements) = do
  typedExpression <- toTypedExpression stack TypeUnknown [untypedExpression]
  (dependencies, result) <- runPrimitive typedExpression
  nextCode <- code stack restStatements
  return (result ++ [Ln ";"] ++ nextCode)

---------------------
-- Void Handler --
---------------------
voidHandler :: TypeHandler
voidHandler stack typeDefinition@((TypeAlgebraicDataType "Void" [])) (Left (selfDependency, code)) =
  Just
    ( do
        return
          ( StackHandler
              { runPrimitive =
                  do
                    return (selfDependency, code),
                runFunctionApplication = \_ -> error "no function application implemented in void",
                runProperty = \_ -> error "no property access implemented",
                runViewStream = \_ -> error "no streaming",
                runResolvedType = typeDefinition,
                runPatternMatching = \_ -> error "no pattern access implemented"
              }
          )
    )
voidHandler _ _ _ = Nothing

-----------------------
-- Component Handler --
-----------------------

componentHandler :: TypeHandler
componentHandler stack typeDefinition@((TypeFunction [typedProperties, _] (TypeAlgebraicDataType "View" []))) (Right [[RightHandSideFunctionDefinition [leftHandSideProperties, leftHandSideAttributes] body]]) =
  Just
    ( do
        return
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
                    propertiesStackHandler <- typedOrigin stack scopedProperties typedProperties
                    stack' <- addToVariableStack stack [(leftHandSideProperties, propertiesStackHandler)]
                    let TypeRecord propertyTypes = typedProperties
                    view <- render stack' body scope (scope ++ [DotNotation "shadowRoot"]) []
                    let dependencies = runViewUpdate view

                    propertySetters <-
                      mapM
                        ( \(propertyName, _) -> do
                            exprId <- getFreshExprId
                            let propertyValue = nameToVariable "propertyValue" exprId
                            let propertyPath = scopedProperties ++ [DotNotation propertyName]
                            let dependency = filter (isPrefixOf propertyPath . fst) dependencies
                            return
                              [ Ln
                                  ("set " ++ propertyName ++ "(" ++ variableToString propertyValue ++ ") {"),
                                Ind
                                  [ Ln
                                      ( variableToString propertyPath
                                          ++ " = "
                                          ++ variableToString propertyValue
                                          ++ ";"
                                      ),
                                    Br,
                                    Ln ("if (" ++ variableToString scopedMounted ++ ") {"),
                                    Ind (concatMap snd dependency),
                                    Ln "}"
                                  ],
                                Ln "}",
                                Br
                              ]
                        )
                        propertyTypes

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
                runViewStream = \_ -> error "no streaming",
                runResolvedType = typeDefinition,
                runPatternMatching = \_ -> error "no pattern access implemented"
              }
          )
    )
componentHandler _ _ _ = Nothing

-----------------
-- Zip Handler --
-----------------

zipHandler :: StackValueContainer
zipHandler stack "zip" =
  Just
    ( ( StackHandler
          { runPrimitive =
              do
                return ([], []),
            runFunctionApplication = \parameters -> do
              parameterPrimitives <- mapM runPrimitive parameters
              let parameterTypes = map runResolvedType parameters

              findType stack (TypeList (TypeTuple parameterTypes)) (Left (concatMap fst parameterPrimitives, Ln "[" : intercalate [Ln ", "] (map snd parameterPrimitives) ++ [Ln "]"])),
            runProperty = \_ -> error "no property access implemented",
            runViewStream = \_ -> error "no streaming",
            runResolvedType = TypeUnknown,
            runPatternMatching = \_ -> error "no pattern access implemented"
          }
      )
    )
zipHandler _ _ = Nothing

prelude :: [StackEntry]
prelude =
  [ StackValue zipHandler,
    StackType operatorHandler,
    StackType booleanHandler,
    StackType numberHandler,
    StackType stringHandler,
    StackType recordHandler,
    StackType listHandler,
    StackType tupleHandler,
    StackType rangeHandler,
    StackType componentHandler,
    StackType functionHandler,
    StackType voidHandler
  ]

------------------------------
-- TypedExpression handling --
------------------------------

toTypedExpression :: Stack -> TypeDefinition -> [UntypedExpression] -> AppStateMonad StackHandler
toTypedExpression stack typeDefinition untypedExpression = do
  toTypedExpression' stack typeDefinition untypedExpression

toTypedExpression' :: Stack -> TypeDefinition -> [UntypedExpression] -> AppStateMonad StackHandler
toTypedExpression' stack typeDefinition [(RightHandSideVariable variableName) : nestedExpressions] = do
  let stackValueContainers = [result' | StackValue stackValueContainer <- stack, let result = stackValueContainer stack variableName, isJust result, let Just result' = result]
  if null stackValueContainers
    then error ("Could not find variable: " ++ variableName)
    else do
      nestedTypedExpression stack (head stackValueContainers) nestedExpressions
toTypedExpression' stack typeDefinition untypedExpression = do
  findType stack typeDefinition (Right untypedExpression)

nestedTypedExpression :: Stack -> StackHandler -> UntypedExpression -> AppStateMonad StackHandler
nestedTypedExpression stack typedExpression [] = do return typedExpression
nestedTypedExpression stack typedExpression ((RightHandSideVariable propertyName) : restUntypedExpression) = do
  typedProperty <- runProperty typedExpression propertyName
  nestedTypedExpression stack typedProperty restUntypedExpression
nestedTypedExpression stack typedExpression ((RightHandSideFunctionCall parameters) : restUntypedExpression) = do
  typedParameters <- mapM (\parameter -> toTypedExpression stack TypeUnknown [parameter]) parameters
  typedProperty <- runFunctionApplication typedExpression typedParameters
  nestedTypedExpression stack typedProperty restUntypedExpression
nestedTypedExpression stack typedExpression untypedExpression = do
  error ("cant nest " ++ show untypedExpression)

findType :: Stack -> TypeDefinition -> StackParameter -> AppStateMonad StackHandler
findType stack = findType' (stack, stack)

findType' :: (Stack, Stack) -> TypeDefinition -> StackParameter -> AppStateMonad StackHandler
findType' ([], _) typeDefinition stackParameter = error ("no corresponding type found " ++ show typeDefinition ++ " + " ++ show stackParameter)
findType' ((StackType stackEntry) : nextStack, allStack) typeDefinition stackParameter =
  case stackEntry allStack typeDefinition stackParameter of
    Just result -> result
    Nothing -> findType' (nextStack, allStack) typeDefinition stackParameter
findType' (_ : nextStack, allStack) typeDefinition stackParameter = findType' (nextStack, allStack) typeDefinition stackParameter

addToVariableStack :: Stack -> [(LeftHandSide, StackHandler)] -> AppStateMonad Stack
addToVariableStack variableStack [] = return variableStack
addToVariableStack variableStack ((LeftHandSideHole, _) : restNewVariables) = addToVariableStack variableStack restNewVariables
addToVariableStack variableStack ((LeftHandSideVariable name, typedExpression) : restNewVariables) = do
  next <- addToVariableStack variableStack restNewVariables
  return (StackValue (\stack name' -> if name == name' then Just typedExpression else Nothing) : next)
addToVariableStack variableStack ((leftHandSide, stackHandler) : restNewVariables) = do
  next <- addToVariableStack variableStack restNewVariables
  result <- runPatternMatching stackHandler leftHandSide
  return (next ++ result)

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
      ( \(propertyName, typeDefinition, groupedProperties) -> do
          let eventPrefix = "on"

          typedPropertyExpression <- toTypedExpression variableStack typeDefinition groupedProperties
          (dependencies, code) <- runPrimitive typedPropertyExpression
          return
            ( if eventPrefix `isPrefixOf` propertyName
                then
                  ( [],
                    Ln (variableToString hostElement ++ ".addEventListener(\"" ++ drop (length eventPrefix) propertyName ++ "\", ") :
                    code
                      ++ [ Ln ");",
                           Br
                         ]
                  )
                else
                  ( dependencies,
                    Ln (variableToString hostElement ++ ".setAttribute(\"" ++ propertyName ++ "\", ") :
                    code
                      ++ [ Ln ");",
                           Br
                         ]
                  )
            )
      )
      (groupProperties properties)

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
render stack ((Stream leftHandSide untypedExpression) : restUntypedBody) scope parent siblings = do
  result <- toTypedExpression stack TypeUnknown [untypedExpression]
  runViewStream result scope parent siblings leftHandSide restUntypedBody
render variableStack ((UntypedExpression [RightHandSideCondition conditionExpression thenStatements elseStatements]) : restUntypedBody) scope parent siblings = do
  exprId <- getFreshExprId
  typedCondition <- toTypedExpression variableStack (TypeAlgebraicDataType "Boolean" []) [conditionExpression]
  (dependencies, typedConditionResult) <- runPrimitive typedCondition

  thenResult <- render variableStack thenStatements scope parent siblings
  elseResult <- render variableStack elseStatements scope parent siblings

  let thenCallback = scope ++ nameToVariable "thenCallback" exprId
  let elseCallback = scope ++ nameToVariable "elseCallback" exprId
  let conditionStorage = scope ++ nameToVariable "conditionStorage" exprId
  let siblings' =
        SiblingCondition [Ln (variableToString conditionStorage)] (runSiblings thenResult) (runSiblings elseResult) :
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
  typedResult <- toTypedExpression variableStack (TypeAlgebraicDataType "String" []) [untypedExpression]
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
            concatMap
              ( \dependency ->
                  [ ( dependency,
                      Ln (variableToString textElement ++ ".textContent = ") : textContent ++ [Ln ";", Br]
                    )
                  ]
              )
              dependencies
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
          Ln "if (" :
          condition
            ++ [ Ln ") {",
                 Ind (siblingsAfter predecessor),
                 Ln "} else {",
                 Ind siblingsNone,
                 Ln "}",
                 Br
               ]
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

typedOrigin :: Stack -> [Variable] -> TypeDefinition -> AppStateMonad StackHandler
typedOrigin stack variablePath typeDefinition = findType stack typeDefinition (Left ([variablePath], [Ln (variableToString variablePath)]))

infers :: TypeDefinition -> TypeDefinition -> TypeDefinition
infers TypeUnknown r = r
infers l r = l
