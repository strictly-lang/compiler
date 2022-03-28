module Emitter.Kinds.View where

import Control.Monad.State.Lazy (MonadState (get), get)
import Data.Char (toUpper)
import Emitter.Types
import Emitter.Util (getGetFreshExprId, nameToVariable, pathToComponentName, variableToString)
import Types

render :: VariableStack -> [Statement] -> Parent -> Predecessor -> AppStateMonad ViewResult
render variableStack untypedBody parent predecessor = error "mep"
