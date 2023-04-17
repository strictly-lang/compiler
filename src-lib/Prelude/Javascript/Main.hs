module Prelude.Javascript.Main (preludedTypehandlerContainer) where

import Prelude.Javascript.Types (JavascriptTypeHandler)
import Prelude.Javascript.Types.Function (typeHandlerContainerFunction)
import TypeChecker.Types (TypeHandlerContainer)

preludedTypehandlerContainer :: [TypeHandlerContainer JavascriptTypeHandler]
preludedTypehandlerContainer = [typeHandlerContainerFunction]

preludedValues = []