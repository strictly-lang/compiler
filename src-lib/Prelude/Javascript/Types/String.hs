module Prelude.Javascript.Types.String where

import Parser.Types (ASTExpression, ASTExpression' (ASTExpressionString), ASTString (ASTStringStatic))
import Prelude.Javascript.Types
import Prelude.Javascript.Util
import TypeChecker.Types (TypeHandlerContext)

javaScriptTypeHandlerStringContainer :: TypeHandlerContext JavaScriptTypeHandler -> ASTExpression -> Maybe JavaScriptTypeHandler
javaScriptTypeHandlerStringContainer types ((ASTExpressionString astStrings) : restExpressions) =
  Just
    JavaScriptTypeHandler
      { getProperty = error "no property access implemented",
        getDom = \renderContext -> do
          return
            [ Ln (runParent renderContext ++ ".append(" ++ concat ['"' : astString ++ ['"'] | ASTStringStatic astString <- astStrings] ++ ");")
            ]
      }
javaScriptTypeHandlerStringContainer types _ = Nothing