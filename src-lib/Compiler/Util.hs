module Compiler.Util
  ( pathToComponent,
    slashToDash,
    slashToCamelCase,
    indent,
    rightHandSideValueToJs,
    functionToJs,
    rightHandSideValueFunctionCallToJs,
    leftHandSideToJs,
    propertyChainToString,
    getGetFreshExprId,
    addImport,
    splitOn,
  )
where

import Compiler.Types
import Control.Monad.State
import Data.Char (toUpper)
import Data.List (intercalate, intersperse, isPrefixOf)
import Data.Maybe (fromMaybe, isNothing)
import Types

type AbsolutePath = String

type ProjectPath = String

pathToComponent :: ProjectPath -> AbsolutePath -> Maybe String
pathToComponent [] (a : as) = Just (removeFileExtension as)
pathToComponent (p : ps) (a : as)
  | p == a = pathToComponent ps as
  | otherwise = Nothing

-- FIX remove magic number
removeFileExtension :: String -> String
removeFileExtension p = take (length p - length ".sly") p

slashToDash :: String -> String
slashToDash [] = []
slashToDash ('/' : ps) = '-' : slashToDash ps
slashToDash (p : ps) = p : slashToDash ps

slashToCamelCase :: String -> String
slashToCamelCase (p : ps) = toUpper p : slashToCamelCase' ps

slashToCamelCase' :: String -> String
slashToCamelCase' [] = []
slashToCamelCase' ('/' : p : ps) = toUpper p : slashToCamelCase' ps
slashToCamelCase' (p : ps) = p : slashToCamelCase' ps

publicVariableToInternal :: VariableStack -> [String] -> InternalVariableName
publicVariableToInternal (stack@(publicStack, internalStack) : vs) search
  | publicStack `isPrefixOf` search = internalStack ++ map DotNotation (drop (length publicStack) search)
  | otherwise = publicVariableToInternal vs search
publicVariableToInternal stack search = error ("Could not find variable for: " ++ show search)

indent :: [Indent] -> String
indent = indent' 0

indent' :: Int -> [Indent] -> String
indent' _ [] = []
indent' indentationLevel (Br : restLines)
  | null restLines = "\n"
  | otherwise = "\n" ++ replicate indentationLevel '\t' ++ indent' indentationLevel restLines
indent' indentationLevel ((Ln line) : restLines) = line ++ indent' indentationLevel restLines
indent' indentationLevel ((Ind indentedLines) : lines) = '\t' : indent' (indentationLevel + 1) indentedLines ++ replicate indentationLevel '\t' ++ indent' indentationLevel lines

functionToJs :: VariableStack -> [RightHandSide] -> AppStateMonad ([Indent], [InternalVariableName])
functionToJs variableStack allFunctions@((FunctionDefinition arguments _) : restFunctionDefinition) = do
  (patterns, dependencies) <- functionToJs' variableStack allFunctions
  let argumentsJs = [[DotNotation ("_arg" ++ show index)] | (_, index) <- zip arguments [0 ..]]
      dependencies' = filter (not . \dependency -> any (`isPrefixOf` dependency) argumentsJs) dependencies
  return
    ( [ Ln ("(" ++ intercalate ", " [propertyChainToString argumentJs | argumentJs <- argumentsJs] ++ ") => {"),
        Br,
        Ind patterns,
        Ln "}"
      ],
      dependencies'
    )
functionToJs variableStack [RightHandSideValue rightHandSideValue] = do
  (functionBodyJs, dependencies) <- rightHandSideValueToJs variableStack rightHandSideValue
  return
    ( [ Ln "(evt) => {",
        Br,
        Ind (functionBodyJs ++ [Ln "(evt)"]),
        Br,
        Ln "}"
      ],
      dependencies
    )

functionToJs' :: VariableStack -> [RightHandSide] -> AppStateMonad ([Indent], [InternalVariableName])
functionToJs' variableStack [] = do return ([], [])
functionToJs' variableStack ((FunctionDefinition arguments rightHandSideValue) : restFunctionDefinition)
  | null patterns = do
    (rightHandValueJs, dependencies) <- rightHandSideValueToJs variableStack'' rightHandSideValue
    return (Ln "return " : rightHandValueJs ++ [Ln ";", Br], dependencies)
  | otherwise = do
    (rightHandValueJs, dependencies) <- rightHandSideValueToJs variableStack'' rightHandSideValue
    (nextPatterns, nextDependencies) <- functionToJs' variableStack restFunctionDefinition
    return
      ( [ Ln "if( "
        ]
          ++ intersperse (Ln " && ") patterns
          ++ [ Ln ") {",
               Br,
               Ind
                 ( Ln "return " :
                   rightHandValueJs ++ [Ln ";"]
                 ),
               Br,
               Ln "}"
             ]
          ++ [Br]
          ++ nextPatterns,
        dependencies ++ nextDependencies
      )
  where
    (patterns, variableStack') = leftHandSidesToJs variableStack [[DotNotation ("_arg" ++ show index)] | index <- [0 ..]] arguments
    variableStack'' = variableStack' ++ variableStack

rightHandSideValueToJs :: VariableStack -> RightHandSideValue -> AppStateMonad ([Indent], [InternalVariableName])
rightHandSideValueToJs variableStack functionCall@(FunctionCall functionReference argumentPublicNames) = rightHandSideValueFunctionCallToJs [] variableStack functionCall
rightHandSideValueToJs variableStack (Variable variableParts) = do
  let variableName = publicVariableToInternal variableStack variableParts
  return ([Ln (propertyChainToString variableName)], [variableName])
rightHandSideValueToJs variableStack (MixedTextValue []) = do return ([Ln ""], [])
rightHandSideValueToJs variableStack (MixedTextValue ((StaticText staticText) : restMixedTextValues))
  | null restMixedTextValues = do return ([Ln ("\"" ++ staticText ++ "\"")], [])
  | otherwise = do
    (restValue, restDependencies) <- rightHandSideValueToJs variableStack (MixedTextValue restMixedTextValues)
    return (Ln ("\"" ++ staticText ++ "\" + ") : restValue, restDependencies)
rightHandSideValueToJs variableStack (MixedTextValue ((DynamicText rightHandSide) : restMixedTextValues))
  | null restMixedTextValues = do
    (currentValue, currentDependencies) <- rightHandSideValueToJs variableStack rightHandSide
    return (currentValue ++ [Ln ".toString()"], currentDependencies)
  | otherwise = do
    (currentValue, currentDependencies) <- rightHandSideValueToJs variableStack rightHandSide
    (restValue, restDependencies) <- rightHandSideValueToJs variableStack (MixedTextValue restMixedTextValues)
    return (currentValue ++ [Ln ".toString() + "] ++ restValue, currentDependencies ++ restDependencies)
rightHandSideValueToJs variableStack (RightHandSideType typeName rightHandSideValues) = do
  rightHandSidesJs <- mapM (rightHandSideValueToJs variableStack) rightHandSideValues
  return
    ( [ Ln ("{ _type: \"" ++ typeName ++ "\", ") -- TODO replace "_type" with Symbol("ADT")
      ]
        ++ ( intercalate
               [Ln ", "]
               [ Ln ("[" ++ show index ++ "]: ") : rightHandSideJs
                 | ((rightHandSideJs, _), index) <-
                     zip rightHandSidesJs [0 ..]
               ]
               ++ [Ln "}"]
           ),
      concatMap snd rightHandSidesJs
    )
rightHandSideValueToJs variableStack (Number number) = do return ([Ln (show number)], [])
rightHandSideValueToJs variableStack (RightHandSideRecord rightHandSideValues msourceRightHandSideValue) = do
  jsValues <- mapM (rightHandSideValueToJs variableStack . snd) rightHandSideValues
  let zipedJsValues = zip (map fst rightHandSideValues) jsValues
  (sourceRightHandSideJs, sourceRightHandSideDependencies) <- case msourceRightHandSideValue of
    Just sourceRightHandSide ->
      do
        (jsValue, dependencies) <- rightHandSideValueToJs variableStack sourceRightHandSide
        return (Ln "..." : jsValue ++ [Ln ","], dependencies)
    Nothing -> do return ([], [])
  return
    ( [Ln "{"]
        ++ sourceRightHandSideJs
        ++ intercalate [Ln ", "] (map (\(propertyName, (jsValue, _)) -> Ln (propertyName ++ ": ") : jsValue) zipedJsValues)
        ++ [ Ln "}"
           ],
      sourceRightHandSideDependencies ++ concatMap snd jsValues
    )
rightHandSideValueToJs variableStack (RightHandSideOperation rightHandSideOperator firstRightHandSideValue secondRightHandSideValue) = do
  (firstRightHandSideJs, firstDependencies) <- rightHandSideValueToJs variableStack firstRightHandSideValue
  (secondRightHandSideJs, secondDpendencies) <- rightHandSideValueToJs variableStack secondRightHandSideValue
  return
    ( firstRightHandSideJs ++ [rightHandSideOperatorToJs rightHandSideOperator] ++ secondRightHandSideJs,
      firstDependencies ++ secondDpendencies
    )
rightHandSideValueToJs variableStack (RightHandSideList rightHandSideValues []) = do
  rightHandSideValuesJs <- mapM (rightHandSideValueToJs variableStack) rightHandSideValues
  return
    ( Ln "[" :
      intercalate [Ln ", "] (map fst rightHandSideValuesJs)
        ++ [ Ln "]"
           ],
      concatMap snd rightHandSideValuesJs
    )
rightHandSideValueToJs variableStack (RightHandSideList rightHandSideValues feedRightHandSideValues) = do
  (content, dependencies) <- rightHandSideListGenerator variableStack rightHandSideValues feedRightHandSideValues
  return
    ( [ Ln "(() => {",
        Br,
        Ind
          ( [ Ln "const result = [];",
              Br
            ]
              ++ content
              ++ [ Br,
                   Ln "return result;",
                   Br
                 ]
          )
      ]
        ++ [ Ln "})()"
           ],
      dependencies
    )
rightHandSideValueToJs variableStack (Tuple rightHandSideValues) = do
  rightHandSideValuesJs <- mapM (rightHandSideValueToJs variableStack) rightHandSideValues

  return
    ( Ln "[" :
      intercalate [Ln ", "] (map fst rightHandSideValuesJs)
        ++ [ Ln "]"
           ],
      concatMap snd rightHandSideValuesJs
    )

rightHandSideOperatorToJs :: RightHandSideOperator -> Indent
rightHandSideOperatorToJs Equal = Ln " == "
rightHandSideOperatorToJs Unequal = Ln " != "
rightHandSideOperatorToJs Plus = Ln " + "
rightHandSideOperatorToJs Minus = Ln " - "
rightHandSideOperatorToJs Multiply = Ln " * "
rightHandSideOperatorToJs Division = Ln " / "
rightHandSideOperatorToJs Modulo = Ln " % "

rightHandSideListGenerator :: VariableStack -> [RightHandSideValue] -> [ListSourceOrFilter] -> AppStateMonad ([Indent], [InternalVariableName])
rightHandSideListGenerator variableStack [] [] = do return ([], [])
rightHandSideListGenerator variableStack (rightHandSideValue : nextRightHandSideValues) [] = do
  (rightHandSideValueJs, rightHandSideDependencies) <- rightHandSideValueToJs variableStack rightHandSideValue
  (nextRightHandSideValueJs, nextRightHandSideDependencies) <- rightHandSideListGenerator variableStack nextRightHandSideValues []
  return
    ( Ln "result.push(" :
      rightHandSideValueJs ++ [Ln ");", Br]
        ++ nextRightHandSideValueJs,
      rightHandSideDependencies ++ nextRightHandSideDependencies
    )
rightHandSideListGenerator variableStack rightHandSideValues ((ListSource (leftHandSide, rightHandSideValue)) : feedRightHandSideValues) = do
  (sourceJs, sourceDependencies) <- rightHandSideValueToJs variableStack rightHandSideValue
  exprId <- getGetFreshExprId
  let variableName = "entity" ++ show exprId
  let (loopConditions, variableStack') = leftHandSideToJs variableStack leftHandSide [DotNotation variableName]
  (nestedJs, nestedDependencies) <- rightHandSideListGenerator (variableStack' ++ variableStack) rightHandSideValues feedRightHandSideValues
  return
    ( Ln ("for (const " ++ variableName ++ " of ") :
      sourceJs
        ++ [ Ln ") {",
             Br,
             Ind
               nestedJs,
             Br,
             Ln "}",
             Br
           ],
      sourceDependencies ++ nestedDependencies
    )
rightHandSideListGenerator variableStack rightHandSideValues (Filter filterRightHandValue : feedRightHandSideValues) = do
  (filterRightHandValueJs, filterRightHandValueDependencies) <- rightHandSideValueToJs variableStack filterRightHandValue
  (nestedJs, nestedDependencies) <- rightHandSideListGenerator variableStack rightHandSideValues feedRightHandSideValues
  return
    ( [ Ln "if ("
      ]
        ++ filterRightHandValueJs
        ++ [ Ln ") {",
             Br,
             Ind
               nestedJs,
             Br,
             Ln "}",
             Br
           ],
      filterRightHandValueDependencies ++ nestedDependencies
    )

type Curry = [Indent]

rightHandSideValueFunctionCallToJs :: [Curry] -> VariableStack -> RightHandSideValue -> AppStateMonad ([Indent], [InternalVariableName])
rightHandSideValueFunctionCallToJs curry variableStack (FunctionCall functionReference argumentPublicNames) = do
  (functionValue, functionDependencies) <- rightHandSideValueToJs variableStack functionReference
  arguments <- mapM (rightHandSideValueToJs variableStack) argumentPublicNames
  let function = functionValue ++ [Ln "("] ++ intercalate [Ln ", "] (curry ++ map fst arguments) ++ [Ln ")"]
  return (function, functionDependencies ++ concatMap snd arguments)

leftHandSidesToJs :: VariableStack -> [InternalVariableName] -> [LeftHandSide] -> ([Indent], VariableStack)
leftHandSidesToJs variableStack _ [] = ([], variableStack)
leftHandSidesToJs variableStack (currentInternalVariableName : restInternalVariableNames) (currentLeftHandSide : restLeftHandSides) =
  let (currentIndents, variableStack') = leftHandSideToJs variableStack currentLeftHandSide currentInternalVariableName
      (restIndentations, variableStack'') = leftHandSidesToJs (variableStack' ++ variableStack) restInternalVariableNames restLeftHandSides
   in (currentIndents ++ restIndentations, variableStack'' ++ variableStack')

leftHandSideToJs :: VariableStack -> LeftHandSide -> InternalVariableName -> ([Indent], VariableStack)
leftHandSideToJs variableStack (LeftAlias name leftHandSide) internalVariableName =
  let (conditions, variableStack') = leftHandSideToJs variableStack leftHandSide internalVariableName
   in (conditions, variableStack' ++ [([name], internalVariableName)])
leftHandSideToJs variableStack (LeftVariable variableName) internalVariableName = ([], [([variableName], internalVariableName)])
leftHandSideToJs variableStack LeftHole internalVariableName = ([], [])
leftHandSideToJs variableStack (LeftTuple leftHandSides) internalVariableName =
  let nestedTupleData = [leftHandSideToJs variableStack leftHandSide (internalVariableName ++ [BracketNotation (show index)]) | (leftHandSide, index) <- zip leftHandSides [0 ..]]
   in (concatMap fst nestedTupleData, concatMap snd nestedTupleData)
leftHandSideToJs variableStack (LeftType typeName leftHandSides) internalVariableName =
  let nestedDataTypes = [leftHandSideToJs variableStack leftHandSide (internalVariableName ++ [BracketNotation (show index)]) | (leftHandSide, index) <- zip leftHandSides [0 ..]]
   in (Ln (propertyChainToString (internalVariableName ++ [DotNotation "_type"]) ++ " == \"" ++ typeName ++ "\"") : concatMap fst nestedDataTypes, concatMap snd nestedDataTypes)
leftHandSideToJs variableStack (LeftRecord properties) internalVariableName =
  let propertiesJs = map (\(property, alias) -> leftHandSideToJs variableStack (fromMaybe (LeftVariable property) alias) (internalVariableName ++ [DotNotation property])) properties
   in (concatMap fst propertiesJs, concatMap snd (reverse propertiesJs))
leftHandSideToJs variableStack (LeftList leftEntities mLeftRest) internalVariableName =
  let leftEntitiesJs = [leftHandSideToJs variableStack leftEntity (internalVariableName ++ [BracketNotation (show index)]) | (leftEntity, index) <- zip leftEntities [0 ..]]
      rest = case mLeftRest of
        Just leftRest ->
          return (leftHandSideToJs variableStack leftRest (internalVariableName ++ [DotNotation ("slice(" ++ show (length leftEntities) ++ ")")]))
        Nothing -> []
   in ( Ln
          ( propertyChainToString (internalVariableName ++ [DotNotation "length"])
              ++ " "
              ++ ( if isNothing mLeftRest
                     then "=="
                     else ">="
                 )
              ++ " "
              ++ show (length leftEntities)
          ) :
        concatMap fst (leftEntitiesJs ++ rest),
        concatMap snd (reverse (leftEntitiesJs ++ rest))
      )

propertyChainToString :: InternalVariableName -> String
propertyChainToString ((DotNotation value) : pcs) = value ++ propertyChainToString' pcs

propertyChainToString' :: InternalVariableName -> String
propertyChainToString' [] = ""
propertyChainToString' ((DotNotation value) : pcs) = '.' : value ++ propertyChainToString' pcs
propertyChainToString' ((BracketNotation value) : pcs) = '[' : value ++ "]" ++ propertyChainToString' pcs

getGetFreshExprId :: AppStateMonad Int
getGetFreshExprId =
  state
    ( \(componentPath, exprId, imports) ->
        (exprId, (componentPath, exprId + 1, imports))
    )

addImport :: Import -> AppStateMonad ()
addImport importPath =
  state
    ( \(componentPath, exprId, imports) ->
        ( (),
          (componentPath, exprId, imports ++ [importPath])
        )
    )

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn f l@(x : xs)
  | f x = splitOn f xs
  | otherwise = let (h, t) = break f l in h : splitOn f t