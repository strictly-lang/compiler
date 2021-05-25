module Compiler.Util (pathToComponent, slashToDash, slashToCamelCase, publicVariableToInternal, indent, filter', rightHandSideValueToJs, functionDefinitionToJs, rightHandSideValueFunctionCallToJs) where

import Compiler.Types
import Data.Char (toUpper)
import Data.List (intercalate, intersperse, isPrefixOf)
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

publicVariableToInternal :: VariableStack -> [String] -> Maybe String
publicVariableToInternal (stack@(publicStack, internalStack) : vs) search
  | publicStack == take (length publicStack) search = Just (intercalate "." (internalStack : drop (length publicStack) search))
  | otherwise = publicVariableToInternal vs search

indent :: [Indent] -> String
indent = indent' 0

indent' :: Int -> [Indent] -> String
indent' _ [] = []
indent' indentationLevel (Br : restLines)
  | null restLines = "\n"
  | otherwise = "\n" ++ replicate indentationLevel '\t' ++ indent' indentationLevel restLines
indent' indentationLevel ((Ln line) : restLines) = line ++ indent' indentationLevel restLines
indent' indentationLevel ((Ind indentedLines) : lines) = '\t' : indent' (indentationLevel + 1) indentedLines ++ replicate indentationLevel '\t' ++ indent' indentationLevel lines

filter' :: (a -> Bool) -> [a] -> ([a], [a])
filter' _ [] = ([], [])
filter' predicate (a : as)
  | matched = (a : nextMatches, nextUnmatches)
  | otherwise = (nextMatches, a : nextUnmatches)
  where
    matched = predicate a
    (nextMatches, nextUnmatches) = filter' predicate as

-- TODO: a compileerror should be thrown instead
unsafeVariable :: Maybe String -> String
unsafeVariable (Just variable) = variable

functionDefinitionToJs :: VariableStack -> [RightHandSide] -> [Indent]
functionDefinitionToJs variableStack allFunctions@((FunctionDefinition arguments _) : restFunctionDefinition) =
  [ Ln ("(" ++ intercalate ", " ["_arg" ++ show index | (_, index) <- zip arguments [0 ..]] ++ ") => {"),
    Br,
    Ind (functionDefinitionToJs' variableStack allFunctions),
    Ln "}"
  ]

functionDefinitionToJs' :: VariableStack -> [RightHandSide] -> [Indent]
functionDefinitionToJs' variableStack [] = []
functionDefinitionToJs' variableStack ((FunctionDefinition arguments rightHandSideValue) : restFunctionDefinition)
  | null patterns = Ln "return " : fst (rightHandSideValueToJs variableStack'' rightHandSideValue) ++ [Br]
  | otherwise =
    [ Ln "if( "
    ]
      ++ intersperse (Ln " && ") patterns
      ++ [ Ln ") {",
           Br,
           Ind
             ( Ln "return " :
               fst (rightHandSideValueToJs variableStack'' rightHandSideValue)
             ),
           Br,
           Ln "}"
         ]
      ++ [Br]
      ++ functionDefinitionToJs' variableStack restFunctionDefinition
  where
    (patterns, variableStack') = leftHandSidesToJs variableStack arguments ["_arg" ++ show index | index <- [0 ..]]
    variableStack'' = variableStack' ++ variableStack

rightHandSideValueToJs :: VariableStack -> RightHandSideValue -> ([Indent], [String])
rightHandSideValueToJs variableStack functionCall@(FunctionCall functionReference argumentPublicNames) = rightHandSideValueFunctionCallToJs [] variableStack functionCall
rightHandSideValueToJs variableStack (Variable variableParts) =
  let variableName = unsafeVariable (publicVariableToInternal variableStack variableParts)
   in ([Ln variableName], [variableName])
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
rightHandSideValueToJs variableStack (RightHandSideType typeName) = ([Ln ("{ _type: \"" ++ typeName ++ "\"}")], [])
rightHandSideValueToJs variableStack (Number number) = ([Ln (show number)], [])
rightHandSideValueToJs variableStack (RightHandSideOperation rightHandSideOperator firstRightHandSideValue secondRightHandSideValue) =
  let (firstRightHandSideJs, firstDependencies) = rightHandSideValueToJs variableStack firstRightHandSideValue
      (secondRightHandSideJs, secondDpendencies) = rightHandSideValueToJs variableStack secondRightHandSideValue
   in ( firstRightHandSideJs ++ [rightHandSideOperatorToJs rightHandSideOperator] ++ secondRightHandSideJs,
        firstDependencies ++ secondDpendencies
      )

rightHandSideOperatorToJs :: RightHandSideOperator -> Indent
rightHandSideOperatorToJs Plus = Ln " + "
rightHandSideOperatorToJs Minus = Ln " - "
rightHandSideOperatorToJs Multiply = Ln " * "
rightHandSideOperatorToJs Division = Ln " / "

type Curry = [Indent]

rightHandSideValueFunctionCallToJs :: [Curry] -> VariableStack -> RightHandSideValue -> ([Indent], [String])
rightHandSideValueFunctionCallToJs curry variableStack (FunctionCall functionReference argumentPublicNames) =
  let (functionValue, functionDependencies) = rightHandSideValueToJs variableStack functionReference
      arguments = map (rightHandSideValueToJs variableStack) argumentPublicNames
      function = functionValue ++ [Ln "("] ++ intercalate [Ln ","] (curry ++ map fst arguments)++ [Ln ")"]
   in (function, functionDependencies ++ concatMap snd arguments)

leftHandSidesToJs :: VariableStack -> [LeftHandSide] -> [InternalVariableName] -> ([Indent], VariableStack)
leftHandSidesToJs variableStack [] _ = ([], variableStack)
leftHandSidesToJs variableStack (currentLeftHandSide : restLeftHandSides) (currentInternalVariableName : restInternalVariableNames) =
  let (currentIndents, variableStack') = leftHandSideToJs variableStack currentLeftHandSide currentInternalVariableName
      (restIndentations, variableStack'') = leftHandSidesToJs (variableStack' ++ variableStack) restLeftHandSides restInternalVariableNames
   in (currentIndents ++ restIndentations, variableStack'' ++ variableStack')

leftHandSideToJs :: VariableStack -> LeftHandSide -> InternalVariableName -> ([Indent], VariableStack)
leftHandSideToJs variableStack (LeftVariable variableName) internalvariableName = ([], [([variableName], internalvariableName)])
leftHandSideToJs variableStack LeftHole internalvariableName = ([], [])
leftHandSideToJs variableStack (LeftType typeName) internalVariableName = ([Ln (internalVariableName ++ "._type === \"" ++ typeName ++ "\"")], [])
