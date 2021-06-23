module Compiler.Util (pathToComponent, slashToDash, slashToCamelCase, indent, rightHandSideValueToJs, functionToJs, rightHandSideValueFunctionCallToJs, leftHandSideToJs, propertyChainToString) where

import Compiler.Types
import Data.Char (toUpper)
import Data.List (intercalate, intersperse, isPrefixOf)
import Data.Maybe (fromMaybe)
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

functionToJs :: VariableStack -> [RightHandSide] -> ([Indent], [InternalVariableName])
functionToJs variableStack allFunctions@((FunctionDefinition arguments _) : restFunctionDefinition) =
  let (patterns, dependencies) = functionToJs' variableStack allFunctions
      argumentsJs = [[DotNotation ("_arg" ++ show index)] | (_, index) <- zip arguments [0 ..]]
      dependencies' = filter (not . \dependency -> any (`isPrefixOf` dependency) argumentsJs) dependencies
   in ( [ Ln ("(" ++ intercalate ", " [propertyChainToString argumentJs | argumentJs <- argumentsJs] ++ ") => {"),
          Br,
          Ind patterns,
          Ln "}"
        ],
        dependencies'
      )
functionToJs variableStack [RightHandSideValue rightHandSideValue] =
  let (functionBodyJs, dependencies) = rightHandSideValueToJs variableStack rightHandSideValue
   in ( [ Ln "(evt) => {",
          Br,
          Ind (functionBodyJs ++ [Ln "(evt)"]),
          Br,
          Ln "}"
        ],
        dependencies
      )

functionToJs' :: VariableStack -> [RightHandSide] -> ([Indent], [InternalVariableName])
functionToJs' variableStack [] = ([], [])
functionToJs' variableStack ((FunctionDefinition arguments rightHandSideValue) : restFunctionDefinition)
  | null patterns = (Ln "return " : rightHandValueJs ++ [Ln ";", Br], dependencies)
  | otherwise =
    let (nextPatterns, nextDependencies) = functionToJs' variableStack restFunctionDefinition
     in ( [ Ln "if( "
          ]
            ++ intersperse (Ln " && ") patterns
            ++ [ Ln ") {",
                 Br,
                 Ind
                   ( Ln "return " :
                     fst (rightHandSideValueToJs variableStack'' rightHandSideValue) ++ [Ln ";"]
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
    (rightHandValueJs, dependencies) = rightHandSideValueToJs variableStack'' rightHandSideValue

rightHandSideValueToJs :: VariableStack -> RightHandSideValue -> ([Indent], [InternalVariableName])
rightHandSideValueToJs variableStack functionCall@(FunctionCall functionReference argumentPublicNames) = rightHandSideValueFunctionCallToJs [] variableStack functionCall
rightHandSideValueToJs variableStack (Variable variableParts) =
  let variableName = publicVariableToInternal variableStack variableParts
   in ([Ln (propertyChainToString variableName)], [variableName])
rightHandSideValueToJs variableStack (MixedTextValue []) = ([Ln ""], [])
rightHandSideValueToJs variableStack (MixedTextValue ((StaticText staticText) : restMixedTextValues))
  | null restMixedTextValues = ([Ln ("\"" ++ staticText ++ "\"")], [])
  | otherwise = (Ln ("\"" ++ staticText ++ "\" + ") : restValue, restDependencies)
  where
    (restValue, restDependencies) = rightHandSideValueToJs variableStack (MixedTextValue restMixedTextValues)
rightHandSideValueToJs variableStack (MixedTextValue ((DynamicText rightHandSide) : restMixedTextValues))
  | null restMixedTextValues = (currentValue ++ [Ln ".toString()"], currentDependencies)
  | otherwise = (currentValue ++ [Ln ".toString() + "] ++ restValue, currentDependencies ++ restDependencies)
  where
    (currentValue, currentDependencies) = rightHandSideValueToJs variableStack rightHandSide
    (restValue, restDependencies) = rightHandSideValueToJs variableStack (MixedTextValue restMixedTextValues)
rightHandSideValueToJs variableStack (RightHandSideType typeName rightHandSideValues) =
  let rightHandSidesJs = map (rightHandSideValueToJs variableStack) rightHandSideValues
   in ( [ Ln ("{ _type: \"" ++ typeName ++ "\",")
        ]
          ++ ( intercalate
                 [Ln ","]
                 [ Ln ("[" ++ show index ++ "]: ") : rightHandSideJs
                   | ((rightHandSideJs, _), index) <-
                       zip rightHandSidesJs [0 ..]
                 ]
                 ++ [Ln "}"]
             ),
        concatMap snd rightHandSidesJs
      )
rightHandSideValueToJs variableStack (Number number) = ([Ln (show number)], [])
rightHandSideValueToJs variableStack (RightHandSideRecord rightHandSideValues msourceRightHandSideValue) =
  let jsValues = map (rightHandSideValueToJs variableStack . snd) rightHandSideValues
      zipedJsValues = zip (map fst rightHandSideValues) jsValues
      (sourceRightHandSideJs, sourceRightHandSideDependencies) = case msourceRightHandSideValue of
        Just sourceRightHandSide ->
          let (jsValue, dependencies) = rightHandSideValueToJs variableStack sourceRightHandSide
           in (Ln "..." : jsValue ++ [Ln ","], dependencies)
        Nothing -> ([], [])
   in ( [Ln "{"]
          ++ sourceRightHandSideJs
          ++ intercalate [Ln ", "] (map (\(propertyName, (jsValue, _)) -> Ln (propertyName ++ ": ") : jsValue) zipedJsValues)
          ++ [ Ln "}"
             ],
        sourceRightHandSideDependencies ++ concatMap snd jsValues
      )
rightHandSideValueToJs variableStack (RightHandSideOperation rightHandSideOperator firstRightHandSideValue secondRightHandSideValue) =
  let (firstRightHandSideJs, firstDependencies) = rightHandSideValueToJs variableStack firstRightHandSideValue
      (secondRightHandSideJs, secondDpendencies) = rightHandSideValueToJs variableStack secondRightHandSideValue
   in ( firstRightHandSideJs ++ [rightHandSideOperatorToJs rightHandSideOperator] ++ secondRightHandSideJs,
        firstDependencies ++ secondDpendencies
      )
rightHandSideValueToJs variableStack (RightHandSideList rightHandSideValues []) =
  let rightHandSideValuesJs = map (rightHandSideValueToJs variableStack) rightHandSideValues
   in ( Ln "[" :
        intercalate [Ln ", "] (map fst rightHandSideValuesJs)
          ++ [ Ln "]"
             ],
        concatMap snd rightHandSideValuesJs
      )
rightHandSideValueToJs variableStack (RightHandSideList rightHandSideValues feedRightHandSideValues) =
  let (content, dependencies) = rightHandSideListGenerator variableStack rightHandSideValues feedRightHandSideValues
   in ( [ Ln "(() => {",
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

rightHandSideOperatorToJs :: RightHandSideOperator -> Indent
rightHandSideOperatorToJs Equal = Ln " == "
rightHandSideOperatorToJs Unequal = Ln " != "
rightHandSideOperatorToJs Plus = Ln " + "
rightHandSideOperatorToJs Minus = Ln " - "
rightHandSideOperatorToJs Multiply = Ln " * "
rightHandSideOperatorToJs Division = Ln " / "
rightHandSideOperatorToJs Modulo = Ln " % "

rightHandSideListGenerator :: VariableStack -> [RightHandSideValue] -> [ListSourceOrFilter] -> ([Indent], [InternalVariableName])
rightHandSideListGenerator variableStack [] [] = ([], [])
rightHandSideListGenerator variableStack (rightHandSideValue : nextRightHandSideValues) [] =
  let (rightHandSideValueJs, rightHandSideDependencies) = rightHandSideValueToJs variableStack rightHandSideValue
      (nextRightHandSideValueJs, nextRightHandSideDependencies) = rightHandSideListGenerator variableStack nextRightHandSideValues []
   in ( Ln "result.push(" :
        rightHandSideValueJs ++ [Ln ");", Br]
          ++ nextRightHandSideValueJs,
        rightHandSideDependencies ++ nextRightHandSideDependencies
      )
rightHandSideListGenerator variableStack rightHandSideValues ((ListSource leftHandSide rightHandSideValue) : feedRightHandSideValues) =
  let (sourceJs, sourceDependencies) = rightHandSideValueToJs variableStack rightHandSideValue
      (loopConditions, variableStack') = leftHandSideToJs variableStack leftHandSide [DotNotation "entity"]
      (nestedJs, nestedDependencies) = rightHandSideListGenerator (variableStack' ++ variableStack) rightHandSideValues feedRightHandSideValues
   in ( Ln "for (const entity of " : -- "entity" variable name needs to be suffixed with exprId
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
rightHandSideListGenerator variableStack rightHandSideValues (Filter filterRightHandValue : feedRightHandSideValues) =
  let (filterRightHandValueJs, filterRightHandValueDependencies) = rightHandSideValueToJs variableStack filterRightHandValue
      (nestedJs, nestedDependencies) = rightHandSideListGenerator variableStack rightHandSideValues feedRightHandSideValues
   in ( [ Ln "if ("
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

rightHandSideValueFunctionCallToJs :: [Curry] -> VariableStack -> RightHandSideValue -> ([Indent], [InternalVariableName])
rightHandSideValueFunctionCallToJs curry variableStack (FunctionCall functionReference argumentPublicNames) =
  let (functionValue, functionDependencies) = rightHandSideValueToJs variableStack functionReference
      arguments = map (rightHandSideValueToJs variableStack) argumentPublicNames
      function = functionValue ++ [Ln "("] ++ intercalate [Ln ", "] (curry ++ map fst arguments) ++ [Ln ")"]
   in (function, functionDependencies ++ concatMap snd arguments)

leftHandSidesToJs :: VariableStack -> [InternalVariableName] -> [LeftHandSide] -> ([Indent], VariableStack)
leftHandSidesToJs variableStack _ [] = ([], variableStack)
leftHandSidesToJs variableStack (currentInternalVariableName : restInternalVariableNames) (currentLeftHandSide : restLeftHandSides) =
  let (currentIndents, variableStack') = leftHandSideToJs variableStack currentLeftHandSide currentInternalVariableName
      (restIndentations, variableStack'') = leftHandSidesToJs (variableStack' ++ variableStack) restInternalVariableNames restLeftHandSides
   in (currentIndents ++ restIndentations, variableStack'' ++ variableStack')

leftHandSideToJs :: VariableStack -> LeftHandSide -> InternalVariableName -> ([Indent], VariableStack)
leftHandSideToJs variableStack (LeftVariable variableName) internalVariableName = ([], [([variableName], internalVariableName)])
leftHandSideToJs variableStack LeftHole internalVariableName = ([], [])
leftHandSideToJs variableStack (LeftTuple leftHandSides) internalVariableName =
  let nestedTupleData = [leftHandSideToJs variableStack leftHandSide (internalVariableName ++ [BracketNotation (show index)]) | (leftHandSide, index) <- zip leftHandSides [0 ..]]
   in ([], concatMap snd nestedTupleData)
leftHandSideToJs variableStack (LeftType typeName leftHandSides) internalVariableName =
  let nestedDataTypes = [leftHandSideToJs variableStack leftHandSide (internalVariableName ++ [BracketNotation (show index)]) | (leftHandSide, index) <- zip leftHandSides [0 ..]]
   in (Ln (propertyChainToString (internalVariableName ++ [DotNotation "_type"]) ++ " == \"" ++ typeName ++ "\"") : concatMap fst nestedDataTypes, concatMap snd nestedDataTypes)
leftHandSideToJs variableStack (LeftRecord properties) internalVariableName =
  let propertiesJs = map (\(property, alias) -> leftHandSideToJs variableStack (fromMaybe (LeftVariable property) alias) (internalVariableName ++ [DotNotation property])) (reverse properties)
   in (concatMap fst propertiesJs, concatMap snd propertiesJs)

propertyChainToString :: InternalVariableName -> String
propertyChainToString ((DotNotation value) : pcs) = value ++ propertyChainToString' pcs

propertyChainToString' :: InternalVariableName -> String
propertyChainToString' [] = ""
propertyChainToString' ((DotNotation value) : pcs) = '.' : value ++ propertyChainToString' pcs
propertyChainToString' ((BracketNotation value) : pcs) = '[' : value ++ "]" ++ propertyChainToString' pcs