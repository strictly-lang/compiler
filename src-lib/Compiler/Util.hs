module Compiler.Util (pathToComponent, slashToDash, slashToCamelCase, publicVariableToInternal) where

import Compiler.Types
import Data.Char (toUpper)
import Data.List (isPrefixOf)

type AbsolutePath = String

type ProjectPath = String

pathToComponent :: ProjectPath -> AbsolutePath -> Maybe String
pathToComponent [] (a : as) = Just (removeFileExtension as)
pathToComponent (p : ps) (a : as)
  | p == a = pathToComponent ps as
  | otherwise = Nothing

removeFileExtension :: String -> String
removeFileExtension p = take (length p - 3) p

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

publicVariableToInternal :: VariableStack -> String -> Maybe String
publicVariableToInternal [] _ = Nothing
publicVariableToInternal (stack@(publicStack, internalStack) : vs) search
  | publicStack == search = Just internalStack
  | (publicStack ++ ".") `isPrefixOf` search = Just (internalStack ++ "." ++ drop (length publicStack + 1) search)
  | otherwise = publicVariableToInternal vs search