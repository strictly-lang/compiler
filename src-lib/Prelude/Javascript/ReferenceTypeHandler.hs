module Prelude.Javascript.ReferenceTypeHandler where

import Prelude.Javascript.Types
import TypeChecker.Types (TypeHandler)

referenceTypeHandlerFactory :: [Code] -> JavaScriptTypeHandler
referenceTypeHandlerFactory reference =
  JavaScriptTypeHandler
    { getExpressionContainer = \renderContext -> do
        return
          JavaScriptExpressionResult
            { getExpressionCode = reference
            }
    }