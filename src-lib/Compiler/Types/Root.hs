module Compiler.Types.Root (compileRoot) where

import Compiler.Types
import Compiler.Types.Model (compileModel)
import Compiler.Types.View (compileView)
import Compiler.Util (filter', indent, slashToCamelCase, slashToDash)
import Data.List (isPrefixOf)
import Types

propertiesScope = "this._properties"

mountedBool = "this._mounted"

compileRoot :: Compiler Root
compileRoot componentPath ast ((View children)) =
  let scope = "this._el"
      (viewContent, _, _, updateCallbacks, _) = compileView children 0 (Context (scope, (["props"], propertiesScope) : getModelScopeVariableStack ast)) "this.shadowRoot" []
   in indent
        [ Ln "(() => {",
          Br,
          Ind
            [ Ln ("class " ++ slashToCamelCase componentPath ++ " extends HTMLElement {"),
              Br,
              Ind
                ( [ Ln "constructor() {",
                    Br,
                    Ind
                      [ Ln "super();",
                        Br,
                        Ln (mountedBool ++ " = false;"),
                        Br,
                        Ln (propertiesScope ++ " = {};"),
                        Br
                      ],
                    Ln "}",
                    Br,
                    Br
                  ]
                    ++ getModelFactories ast
                    ++ [ Br,
                         Ln "connectedCallback() {",
                         Br,
                         Ind
                           ( [ Ln (mountedBool ++ " = true;"),
                               Br,
                               Ln (scope ++ " = {};"),
                               Br,
                               Ln "this.attachShadow({mode: 'open'});",
                               Br
                             ]
                               ++ viewContent
                           ),
                         Ln "}",
                         Br,
                         Br
                       ]
                    ++ walkDependencies updateCallbacks
                ),
              Ln "}",
              Br,
              Br,
              Ln ("customElements.define(\"" ++ slashToDash componentPath ++ "\", " ++ slashToCamelCase componentPath ++ ");"),
              Br
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
walkDependencies (UpdateCallbacks ((internalName, updateCallback) : updateCallbacks))
  | isProps =
    let setterName = internalNameToSetterName internalName
        (matchedUpdateCallbacks, unmatchedUpdateCallbacks) = filter' ((setterName ==) . internalNameToSetterName . fst) updateCallbacks
     in getSetter setterName (updateCallback : map snd matchedUpdateCallbacks) ++ walkDependencies (UpdateCallbacks unmatchedUpdateCallbacks)
  | otherwise = error ("There is an observer missing for " ++ internalName)
  where
    isProps = (propertiesScope ++ ".") `isPrefixOf` internalName

internalNameToSetterName :: String -> String
internalNameToSetterName internalName = head (drop (length (splitByDot propertiesScope)) (splitByDot internalName))

getSetter :: String -> [[Indent]] -> [Indent]
getSetter name updateCallback =
  [ Ln ("set " ++ name ++ "(value) {"),
    Br,
    Ind
      [ Ln (propertiesScope ++ "." ++ name ++ " = value;"),
        Br,
        Ln ("if (" ++ mountedBool ++ ") {"),
        Br,
        Ind (concat updateCallback),
        Br,
        Ln "}",
        Br
      ],
    Ln "}",
    Br,
    Br
  ]