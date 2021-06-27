module Compiler.Types.View.Host where

import Compiler.Types
import Compiler.Util (addImport, functionToJs, getGetFreshExprId, propertyChainToString, rightHandSideValueToJs, splitOn)
import Control.Monad.State
import Data.Char (toLower)
import Data.List (intercalate, partition)
import Types

compileHost :: Context -> String -> HostElement -> Maybe Import -> AppStateMonad (HostElement, [Indent], UpdateCallbacks)
compileHost (Context (scope, variableStack)) elementVariable (HostElement ("input", options, children)) importPath =
  do
    exprId <- getGetFreshExprId
    let ([("value", [value])], options') = partition ((== "value") . fst) options
        ([oninput], options'') = partition ((== "oninput") . fst) options'
        (typeName, valueJs, dependencies) = getTypeAndValue variableStack value
        valueAttribute = getValueAttributeOfType typeName
        valueVariable = propertyChainToString scope ++ ".valueContainer" ++ show exprId
        valueChanged = propertyChainToString scope ++ ".valueChanged" ++ show exprId
        (functionJs, functionDependencies) = functionToJs variableStack (snd oninput)
    return
      ( HostElement ("input", options'', children),
        [ Ln (elementVariable ++ ".setAttribute(\"type\", \"" ++ toLowerCase typeName ++ "\");"),
          Br,
          Ln (valueChanged ++ " = false;"),
          Br,
          Ln (valueVariable ++ " = ")
        ]
          ++ valueJs
          ++ [ Ln ";",
               Br,
               Ln (elementVariable ++ "." ++ valueAttribute ++ " = " ++ valueVariable ++ ";"),
               Br,
               Ln (elementVariable ++ ".addEventListener(\"input\", (evt) => {"),
               Br,
               Ind
                 ( [ Ln (valueChanged ++ " = false;"),
                     Br,
                     Ln "("
                   ]
                     ++ functionJs
                     ++ [ Ln (")({ _type: \"" ++ typeName ++ "\", [0]: evt.currentTarget." ++ valueAttribute ++ "});"),
                          Br,
                          Br,
                          Ln ("if (" ++ valueChanged ++ " === false) {"),
                          Br,
                          Ind
                            [ Ln (elementVariable ++ "." ++ valueAttribute ++ " = " ++ valueVariable ++ ";")
                            ],
                          Br,
                          Ln "}",
                          Br
                        ]
                 ),
               Br,
               Ln "});",
               Br
             ],
        UpdateCallbacks
          ( [ ( dependency,
                [ Ln (valueChanged ++ " = true;"),
                  Br,
                  Ln (valueVariable ++ " = ")
                ]
                  ++ valueJs
                  ++ [ Ln ";",
                       Br,
                       Ln (elementVariable ++ "." ++ valueAttribute ++ " = " ++ valueVariable ++ ";"),
                       Br
                     ]
              )
              | dependency <- dependencies
            ]
              ++ [(functionDependency, []) | functionDependency <- functionDependencies]
          )
      )
compileHost context elementVariable host@(HostElement (hostElementName, mergedOptions, children)) (Just importPath@(Import ('.' : '/' : _, []))) =
  do
    _ <- addImport importPath
    (currentComponentPath, _, _) <- get
    return (HostElement (getRelativeComponentName currentComponentPath hostElementName, mergedOptions, children), [], UpdateCallbacks [])
compileHost context elementVariable host@(HostElement hostElementName) (Just importPath) =
  do
    _ <- addImport importPath
    return (host, [], UpdateCallbacks [])
compileHost context elementVariable host importPath = do return (host, [], UpdateCallbacks [])

getTypeAndValue :: VariableStack -> RightHandSide -> (String, [Indent], [InternalVariableName])
getTypeAndValue variableStack (RightHandSideValue (RightHandSideType typeName [rightHandSideValue])) =
  let (rightHandSideJs, dependencies) = rightHandSideValueToJs variableStack rightHandSideValue
   in (typeName, rightHandSideJs, dependencies)

getValueAttributeOfType :: String -> String
getValueAttributeOfType "CheckBox" = "checked"
getValueAttributeOfType _ = "value"

toLowerCase :: String -> String
toLowerCase value = [toLower loweredString | loweredString <- value]

getRelativeComponentName :: String -> String -> String
getRelativeComponentName currentComponentName hostElementName =
  let splitedCurrentComponentName = splitOn (== '/') currentComponentName
   in intercalate "-" (take (length splitedCurrentComponentName - 1) splitedCurrentComponentName) ++ "-" ++ hostElementName