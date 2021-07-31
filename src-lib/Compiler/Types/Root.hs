module Compiler.Types.Root (compileRoot) where

import Compiler.Types
import Compiler.Types.Model.Base (compileModel)
import Compiler.Types.View.Base (compileView)
import Compiler.Util (indent, propertyChainToString, slashToCamelCase, slashToDash)
import Control.Monad.State
import Data.List (intercalate, isPrefixOf, partition)
import Types

propertiesScope = [DotNotation "this", DotNotation "properties"]

mountedBool = "this._mounted"

startState :: String -> AppState
startState componentPath = (componentPath, 0, [])

compileRoot :: String -> [Root] -> String
compileRoot componentPath ast =
  let (result, (_, _, imports)) = runState (compileRoot' componentPath ast ast []) (startState componentPath)
   in indent
        ( concat
            ( [ [ Ln ("import \"" ++ path ++ "\";"),
                  Br
                ]
                | Import (path, _) <- imports
              ]
            )
            ++ result
        )

compileRoot' :: String -> [Root] -> [Root] -> VariableStack -> AppStateMonad [Indent]
compileRoot' componentPath [] ast variableStack = do return []
compileRoot' componentPath ((RootImport (Import (path, imports))) : ns) ast variableStack = do
  next <- compileRoot' componentPath ns ast ([([importVariable], [DotNotation importVariable]) | importVariable <- imports] ++ variableStack)
  return
    ( [ Ln ("import { " ++ intercalate ", " imports ++ " } from \"" ++ path ++ "\";"),
        Br
      ]
        ++ next
    )
compileRoot' componentPath ((View children) : ns) ast variableStack = do
  let scope = [DotNotation "this", DotNotation "_el"]
      variableStack' = getModelScopeVariableStack ast ++ variableStack
  (viewContent, _, UpdateCallbacks updateCallbacks, _) <- compileView children (Context (scope, (["props"], propertiesScope) : variableStack')) "this.shadowRoot" []
  next <- compileRoot' componentPath ns ast variableStack

  return
    ( [ Ln "(() => {",
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
                      Ln (propertyChainToString propertiesScope ++ " = {};"),
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
                             Ln (propertyChainToString scope ++ " = {};"),
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
        ++ next
    )
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
getModelScopeVariableStack ((Model name _) : restRoot) = ([name], [DotNotation "this", DotNotation name]) : getModelScopeVariableStack restRoot
getModelScopeVariableStack (currentRoot : restRoot) = getModelScopeVariableStack restRoot

walkDependencies :: UpdateCallbacks -> [Indent]
walkDependencies (UpdateCallbacks []) = []
walkDependencies (UpdateCallbacks ((internalName, updateCallback) : updateCallbacks))
  | isProps =
    let setterName = internalNameToSetterName internalName
        (matchedUpdateCallbacks, unmatchedUpdateCallbacks) = partition ((setterName ==) . internalNameToSetterName . fst) updateCallbacks
     in getSetter setterName (updateCallback : map snd matchedUpdateCallbacks) ++ walkDependencies (UpdateCallbacks unmatchedUpdateCallbacks)
  | otherwise = error ("There is an observer missing for " ++ propertyChainToString internalName)
  where
    isProps = propertiesScope `isPrefixOf` internalName

internalNameToSetterName :: InternalVariableName -> String
internalNameToSetterName internalName
  | propertiesScope `isPrefixOf` internalName =
    let DotNotation setterName = head (drop (length propertiesScope) internalName)
     in setterName
  | otherwise = error ("There is an observer missing for " ++ propertyChainToString internalName)

getSetter :: String -> [[Indent]] -> [Indent]
getSetter name updateCallback =
  [ Ln ("set " ++ name ++ "(value) {"),
    Br,
    Ind
      [ Ln (propertyChainToString (propertiesScope ++ [DotNotation name]) ++ " = value;"),
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