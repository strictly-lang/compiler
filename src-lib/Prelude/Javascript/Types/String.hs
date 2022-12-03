module Prelude.Javascript.Types.String where

import Parser.Types (ASTExpression, ASTExpression' (ASTExpressionString), ASTString (ASTStringStatic))
import Prelude.Javascript.Types
import Prelude.Javascript.Util

javaScriptTypeHandlerStringContainer :: ASTExpression -> Maybe JavaScriptTypeHandler
javaScriptTypeHandlerStringContainer ((ASTExpressionString astStrings) : restExpressions) =
  Just
    JavaScriptTypeHandler
      { getProperty = error "no property access implemented",
        getDom = \parent -> [Ln (parent ++ ".append(" ++ concat ['"' : astString ++ ['"'] | ASTStringStatic astString <- astStrings] ++ ");")]
      }
javaScriptTypeHandlerStringContainer _ = Nothing