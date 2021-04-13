module Compiler.Types.Root (compileRoot) where

import Compiler.Types
import Compiler.Types.View (compileView)
import Compiler.Util (indent, slashToCamelCase, slashToDash)
import Types

propertiesScope = "this._properties"

mountedBool = "this._mounted"

compileRoot :: Compiler Root
compileRoot componentPath ast (Node exprId (View children)) =
  let scope = "this._el"
      (viewContent, _, updateCallbacks) = compileView children (Context (scope, [("props", propertiesScope)])) "this.shadowRoot" FirstElement
   in unlines
        ( ( "(() => {" :
            indent
              ( ("class " ++ slashToCamelCase componentPath ++ " extends HTMLElement {") :
                indent
                  ( ( "constructor() {" :
                      indent
                        [ "super();",
                          mountedBool ++ " = false;",
                          propertiesScope ++ " = {};"
                        ]
                        ++ ["}"]
                    )
                      ++ [ "connectedCallback() {"
                         ]
                      ++ indent
                        [ mountedBool ++ " = true;",
                          scope ++ " = {};",
                          "this.attachShadow({mode: 'open'});"
                        ]
                      ++ indent viewContent
                      ++ ["}"]
                  )
                  ++ indent (walkDependencies updateCallbacks)
                  ++ ["}"]
                  ++ ["customElements.define(\"" ++ slashToDash componentPath ++ "\"," ++ slashToCamelCase componentPath ++ ");"]
              )
          )
            ++ ["})()"]
        )
compileRoot _ _ _ = ""

splitBy :: (Foldable t, Eq a) => a -> t a -> [[a]]
splitBy delimiter = foldr f [[]]
  where
    f c l@(x : xs)
      | c == delimiter = [] : l
      | otherwise = (c : x) : xs

splitByDot = splitBy '.'

walkDependencies :: UpdateCallbacks -> [String]
walkDependencies (UpdateCallbacks []) = []
walkDependencies (UpdateCallbacks ((internalName, updateCallback) : updateCallbacks)) =
  let setterName = internalNameToSetterName internalName
      (matchedUpdateCallbacks, unmatchedUpdateCallbacks) = filter' ((setterName ==) . internalNameToSetterName . fst) updateCallbacks
   in getSetter setterName (unlines (updateCallback : map snd matchedUpdateCallbacks)) : walkDependencies (UpdateCallbacks unmatchedUpdateCallbacks)

internalNameToSetterName :: String -> String
internalNameToSetterName internalName = head (drop (length (splitByDot propertiesScope)) (splitByDot internalName))

filter' :: (a -> Bool) -> [a] -> ([a], [a])
filter' _ [] = ([], [])
filter' predicate (a : as)
  | matched = (a : nextMatches, nextUnmatches)
  | otherwise = (nextMatches, a : nextUnmatches)
  where
    matched = predicate a
    (nextMatches, nextUnmatches) = filter' predicate as

getSetter :: String -> String -> String
getSetter name updateCallback =
  "\
  \   set "
    ++ name
    ++ "(value) {\n\
       \       "
    ++ propertiesScope
    ++ "."
    ++ name
    ++ " = value\n\
       \       if ("
    ++ mountedBool
    ++ ") {\n\
       \           "
    ++ updateCallback
    ++ "\n\
       \       }\n\
       \   }"