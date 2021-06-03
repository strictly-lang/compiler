module Compiler.Types.View.Host where

import Compiler.Types
import Compiler.Util (filter', functionToJs, rightHandSideValueToJs)
import Data.Char (toLower)
import Types

compileHost :: Context -> ExprId -> String -> ViewContent -> (ViewContent, [Indent], UpdateCallbacks)
compileHost (Context (scope, variableStack)) exprId elementVariable (Host "input" options children) =
  let ([("value", [value])], options') = filter' ((== "value") . fst) options
      ([oninput], options'') = filter' ((== "oninput") . fst) options'
      (typeName, valueJs, dependencies) = getTypeAndValue variableStack value
      valueAttribute = getValueAttributeOfType typeName
      valueVariable = scope ++ ".valueContainer" ++ show exprId
      valueChanged = scope ++ ".valueChanged" ++ show exprId
      (functionJs, functionDependencies) = functionToJs variableStack (snd oninput)
   in ( Host "input" options'' children,
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
                     ++ [ Ln (")({ _type: \"" ++ typeName ++ "\", _dt0: evt.currentTarget." ++ valueAttribute ++ "});"),
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
compileHost context exprId elementVariable host@Host {} = (host, [], UpdateCallbacks [])

getTypeAndValue :: VariableStack -> RightHandSide -> (String, [Indent], [InternalVariableName])
getTypeAndValue variableStack (RightHandSideValue (RightHandSideType typeName [rightHandSideValue])) =
  let (rightHandSideJs, dependencies) = rightHandSideValueToJs variableStack rightHandSideValue
   in (typeName, rightHandSideJs, dependencies)

getValueAttributeOfType :: String -> String
getValueAttributeOfType "CheckBox" = "checked"
getValueAttributeOfType _ = "value"

toLowerCase :: String -> String
toLowerCase value = [toLower loweredString | loweredString <- value]