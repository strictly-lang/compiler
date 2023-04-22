module Prelude.Javascript.Main (preludedTypehandlerContainer) where

import Parser.Types (ASTExpression', ASTTypeDeclaration)
import Prelude.Javascript.Types
import Prelude.Javascript.Types.Function (typeHandlerContainerFunction)
import TypeChecker.Types (TypeHandlerContainer)

preludedTypehandlerContainer :: [TypeHandlerContainer JavascriptTypeHandler]
preludedTypehandlerContainer = [typeHandlerContainerFunction]

preludedValues = []