module Compiler.Util where

import Compiler.Types
import Control.Monad.State.Lazy (MonadState (state))

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
