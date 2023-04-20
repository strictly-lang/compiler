module Prelude.Javascript.Main (preludedTypehandlerContainer) where

import Prelude.Javascript.Types
import Prelude.Javascript.Types.Function (typeHandlerContainerFunction)
import TypeChecker.Types

preludedTypehandlerContainer :: [TypeHandlerContainer JavascriptTypeHandler]
preludedTypehandlerContainer = [typeHandlerContainerFunction]

preludedValues = []