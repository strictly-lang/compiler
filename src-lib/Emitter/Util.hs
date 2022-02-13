module Emitter.Util where

import Control.Monad.State.Lazy (MonadState (state))
import Emitter.Types

type AbsolutePath = String

type ProjectPath = String

pathToComponentName :: ProjectPath -> AbsolutePath -> Maybe String
pathToComponentName [] (a : as) = Just (removeFileExtension as)
pathToComponentName (p : ps) (a : as)
  | p == a = pathToComponentName ps as
  | otherwise = Nothing

getGetFreshExprId :: AppStateMonad Int
getGetFreshExprId =
  state
    ( \(AppState componentPath expressionId) ->
        (expressionId, AppState componentPath (expressionId + 1))
    )

removeFileExtension :: String -> String
removeFileExtension p = take (length p - length ".sly") p

data Variable = DotNotation String | BracketNotation String

instance Show Variable where
  show (DotNotation name) = '.' : name
  show (BracketNotation name) = '[' : name ++ "]"

nameToVariable :: String -> Int -> [Variable]
nameToVariable name exprId = [DotNotation (name ++ show exprId)]

variableToString :: [Variable] -> String
variableToString ((DotNotation v) : vs) = concat (v : map show vs)
