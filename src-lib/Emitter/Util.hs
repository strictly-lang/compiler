module Emitter.Util where

import Control.Monad.State.Lazy (MonadState (state))
import Data.Char (toUpper)
import Emitter.Types
import Types

type AbsolutePath = String

type ProjectPath = String

pathToComponentName :: ProjectPath -> AbsolutePath -> Maybe String
pathToComponentName [] (a : as) = Just (removeFileExtension as)
pathToComponentName (p : ps) (a : as)
  | p == a = pathToComponentName ps as
  | otherwise = Nothing

getFreshExprId :: AppStateMonad Int
getFreshExprId =
  state
    ( \(AppState componentPath expressionId) ->
        (expressionId, AppState componentPath (expressionId + 1))
    )

removeFileExtension :: String -> String
removeFileExtension p = take (length p - length ".sly") p

instance Show Variable where
  show (DotNotation name) = '.' : name
  show (BracketNotation name) = '[' : name ++ "]"

nameToVariable :: String -> Int -> [Variable]
nameToVariable name exprId = [DotNotation (name ++ show exprId)]

variableToString :: [Variable] -> String
variableToString ((DotNotation v) : vs) = concat (v : map show vs)

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