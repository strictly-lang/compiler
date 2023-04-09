module Prelude.Javascript.Types.Function where

import Parser.Types (ASTExpression')
import Prelude.Javascript.Types

typeHandlerContainerFunction :: ASTExpression' -> Maybe JavascriptTypeHandler
typeHandlerContainerFunction expression = Nothing