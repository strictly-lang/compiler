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
      (viewContent, _, updateCodes) = compileView children (Context (scope, [("props", propertiesScope)])) "this.shadowRoot" FirstElement
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
                  ++ indent (walkDependencies updateCodes)
                  ++ ["}"]
                  ++ ["customElements.define(\"" ++ slashToDash componentPath ++ "\"," ++ slashToCamelCase componentPath ++ ");"]
              )
          )
            ++ ["})()"]
        )
compileRoot _ _ _ = ""

splitBy delimiter = foldr f [[]]
  where
    f c l@(x : xs)
      | c == delimiter = [] : l
      | otherwise = (c : x) : xs

splitByDot = splitBy '.'

walkDependencies :: UpdateCodes -> [String]
walkDependencies [] = []
walkDependencies ((internalName, updateCode) : updateCodes) =
  let setterName = internalNameToSetterName internalName
      (matchedUpdateCodes, unmatchedUpdateCodes) = filter' ((setterName ==) . internalNameToSetterName . fst) updateCodes
   in getSetter setterName (unlines (updateCode : map snd matchedUpdateCodes)) : walkDependencies unmatchedUpdateCodes

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
getSetter name updateCode =
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
    ++ updateCode
    ++ "\n\
       \       }\n\
       \   }"