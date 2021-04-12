module Compiler.Types.Root (compileRoot) where

import Types

import Compiler.Types
import Compiler.Types.View ( compileView )
import Compiler.Util (slashToDash, slashToCamelCase )

propertiesScope = "this._properties";
mountedBool = "this._mounted";

compileRoot :: Compiler Root
compileRoot componentPath ast (Node exprId (View children)) =
    let 
        (viewContent, _, updateCodes) = compileView children (Context [("props", propertiesScope)])"this.shadowRoot" FirstElement

    in "\
\(() => {\n\
\class " ++ slashToCamelCase componentPath ++ " extends HTMLElement {\n\
\    constructor() {\n\
\       super();\n\
\       " ++ mountedBool ++ " = false;\n\
\       " ++ propertiesScope ++ " = {};\n\
\    }\n\
\    connectedCallback() {\n\
\       " ++ mountedBool ++ " = true;\n\
\       this.attachShadow({mode: 'open'});\n\
\" ++  viewContent ++ "\n\
\    }\n\
\" ++ unlines (walkDependencies updateCodes) ++ "\
\}\n\
\customElements.define(\"" ++ slashToDash componentPath ++ "\", " ++ slashToCamelCase componentPath ++ ");\n\
\})()\n";
compileRoot _ _ _ = ""

splitBy delimiter = foldr f [[]] 
            where f c l@(x:xs) | c == delimiter = []:l
                             | otherwise = (c:x):xs

splitByDot = splitBy '.'

walkDependencies :: UpdateCodes -> [String]
walkDependencies [] = []
walkDependencies ((internalName, updateCode):updateCodes) =
    let
        setterName = internalNameToSetterName internalName
        (matchedUpdateCodes, unmatchedUpdateCodes) = filter' ((setterName ==) . internalNameToSetterName .fst) updateCodes
    in getSetter setterName (unlines (updateCode : map snd matchedUpdateCodes)) : walkDependencies unmatchedUpdateCodes

internalNameToSetterName :: String -> String 
internalNameToSetterName internalName = head (drop (length (splitByDot propertiesScope)) (splitByDot internalName));

filter' :: (a -> Bool) -> [a] -> ([a], [a])
filter' _ []= ([], [])
filter' predicate (a:as)
    | matched = (a : nextMatches, nextUnmatches)
    | otherwise = (nextMatches, a:nextUnmatches)
    where
        matched = predicate a
        (nextMatches, nextUnmatches) = filter' predicate as

getSetter :: String -> String -> String 
getSetter name updateCode= "\
\   set " ++ name ++ "(value) {\n\
\       "++ propertiesScope ++"."++ name ++ " = value\n\
\       if (" ++ mountedBool ++ ") {\n\
\           " ++ updateCode ++ "\n\
\       }\n\
\   }"