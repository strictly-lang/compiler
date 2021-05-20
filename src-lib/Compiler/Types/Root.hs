module Compiler.Types.Root (compileRoot) where

import Compiler.Types
import Compiler.Types.Model (compileModel)
import Compiler.Types.View (compileView)
import Compiler.Util (filter', indent, slashToCamelCase, slashToDash)
import Types

propertiesScope = "this._properties"

mountedBool = "this._mounted"

compileRoot :: Compiler Root
compileRoot componentPath ast ((View children)) =
  let scope = "this._el"
      (viewContent, _, _, updateCallbacks, _) = compileView children 0 (Context (scope, (["props"], propertiesScope) : getModelScopeVariableStack ast)) "this.shadowRoot" []
   in indent
        [ Ln "(() => {",
          Ind
            [ Ln ("class " ++ slashToCamelCase componentPath ++ " extends HTMLElement {"),
              Ind
                ( [ Ln "constructor() {",
                    Ind
                      [ Ln "super();",
                        Ln (mountedBool ++ " = false;"),
                        Ln (propertiesScope ++ " = {};")
                      ],
                    Ln "}",
                    Ln ""
                  ]
                    ++ getModelFactories ast
                    ++ [ Ln "",
                         Ln "connectedCallback() {",
                         Ind
                           ( [ Ln (mountedBool ++ " = true;"),
                               Ln (scope ++ " = {};"),
                               Ln "this.attachShadow({mode: 'open'});"
                             ]
                               ++ viewContent
                           ),
                         Ln "}",
                         Ln ""
                       ]
                    ++ walkDependencies updateCallbacks
                ),
              Ln "}",
              Ln ("customElements.define(\"" ++ slashToDash componentPath ++ "\"," ++ slashToCamelCase componentPath ++ ");")
            ],
          Ln "})()"
        ]
compileRoot _ _ _ = ""

getModelFactories :: [Root] -> [Indent]
getModelFactories [] = []
getModelFactories (model@(Model _ _) : rest) = compileModel model ++ getModelFactories rest
getModelFactories (_ : rest) = getModelFactories rest

splitBy :: (Foldable t, Eq a) => a -> t a -> [[a]]
splitBy delimiter = foldr f [[]]
  where
    f c l@(x : xs)
      | c == delimiter = [] : l
      | otherwise = (c : x) : xs

getModelScopeVariableStack :: [Root] -> [([PublicVariableName], InternalVariableName)]
getModelScopeVariableStack [] = []
getModelScopeVariableStack ((Model name _) : restRoot) = ([name], "this." ++ name) : getModelScopeVariableStack restRoot
getModelScopeVariableStack (currentRoot : restRoot) = getModelScopeVariableStack restRoot

splitByDot = splitBy '.'

walkDependencies :: UpdateCallbacks -> [Indent]
walkDependencies (UpdateCallbacks []) = []
walkDependencies (UpdateCallbacks ((internalName, updateCallback) : updateCallbacks)) =
  let setterName = internalNameToSetterName internalName
      (matchedUpdateCallbacks, unmatchedUpdateCallbacks) = filter' ((setterName ==) . internalNameToSetterName . fst) updateCallbacks
   in getSetter setterName (updateCallback : map snd matchedUpdateCallbacks) ++ walkDependencies (UpdateCallbacks unmatchedUpdateCallbacks)

internalNameToSetterName :: String -> String
internalNameToSetterName internalName = head (drop (length (splitByDot propertiesScope)) (splitByDot internalName))

getSetter :: String -> [[Indent]] -> [Indent]
getSetter name updateCallback =
  [ Ln ("set " ++ name ++ "(value) {"),
    Ind
      [ Ln (propertiesScope ++ "." ++ name ++ " = value;"),
        Ln ("if (" ++ mountedBool ++ ") {"),
        Ind (concat updateCallback),
        Ln "}"
      ],
    Ln "}",
    Ln ""
  ]