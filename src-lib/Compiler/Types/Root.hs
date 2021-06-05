module Compiler.Types.Root (compileRoot) where

import Compiler.Types
import Compiler.Types.Model (compileModel)
import Compiler.Types.View.Base (compileView)
import Compiler.Util (indent, slashToCamelCase, slashToDash)
import Data.List (intercalate, isPrefixOf, partition)
import Types

propertiesScope = "this._properties"

mountedBool = "this._mounted"

compileRoot :: String -> [Root] -> String
compileRoot componentPath ast = indent (compileRoot' componentPath ast ast [])

compileRoot' :: String -> [Root] -> [Root] -> VariableStack -> [Indent]
compileRoot' componentPath [] ast variableStack = []
compileRoot' componentPath ((Import path imports) : ns) ast variableStack =
  [ Ln ("import { " ++ intercalate ", " imports ++ " } from \"" ++ path ++ "\";"),
    Br
  ]
    ++ compileRoot' componentPath ns ast ([([importVariable], importVariable) | importVariable <- imports] ++ variableStack)
compileRoot' componentPath ((View children) : ns) ast variableStack =
  let scope = "this._el"
      variableStack' = getModelScopeVariableStack ast ++ variableStack
      (viewContent, _, _, UpdateCallbacks updateCallbacks, _) = compileView children 0 (Context (scope, (["props"], propertiesScope) : variableStack')) "this.shadowRoot" []
   in [ Ln "(() => {",
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
                  ++ getModelFactories ast variableStack
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
                  ++ walkDependencies (UpdateCallbacks (filter (not . ((\value -> value `elem` map snd variableStack') . fst)) updateCallbacks))
              ),
            Ln "}",
            Br,
            Br,
            Ln ("customElements.define(\"" ++ slashToDash componentPath ++ "\", " ++ slashToCamelCase componentPath ++ ");"),
            Br
          ],
        Ln "})()"
      ]
        ++ compileRoot' componentPath ns ast variableStack
compileRoot' componentPath ((Model _ _) : ns) ast variableStack = compileRoot' componentPath ns ast variableStack

getModelFactories :: [Root] -> VariableStack -> [Indent]
getModelFactories [] _ = []
getModelFactories (model@(Model _ _) : rest) variableStack = compileModel model variableStack ++ getModelFactories rest variableStack
getModelFactories (_ : rest) variableStack = getModelFactories rest variableStack

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
        (matchedUpdateCallbacks, unmatchedUpdateCallbacks) = partition ((setterName ==) . internalNameToSetterName . fst) updateCallbacks
     in getSetter setterName (updateCallback : map snd matchedUpdateCallbacks) ++ walkDependencies (UpdateCallbacks unmatchedUpdateCallbacks)
  | otherwise = error ("There is an observer missing for " ++ internalName)
  where
    isProps = (propertiesScope ++ ".") `isPrefixOf` internalName

internalNameToSetterName :: String -> String
internalNameToSetterName internalName
  | splitPropertiesScope `isPrefixOf` splitInternalName = head (drop (length splitPropertiesScope) splitInternalName)
  | otherwise = error ("There is an observer missing for " ++ internalName)
  where
    splitInternalName = splitByDot internalName
    splitPropertiesScope = splitByDot propertiesScope

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