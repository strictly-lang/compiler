module Prelude.Javascript.Types.Host where

import Parser.Types (ASTExpression, ASTExpression' (ASTExpressionHost, ASTExpressionString), ASTString (ASTStringStatic))
import Prelude.Javascript.Types
import Prelude.Javascript.Util

javaScriptTypeHandlerHostContainer :: ASTExpression -> Maybe JavaScriptTypeHandler
javaScriptTypeHandlerHostContainer [ASTExpressionHost hostName attributes children] =
  Just
    JavaScriptTypeHandler
      { getProperty = error "no property access implemented",
        getDom = \parent ->
          [ Ln (parent ++ ".append(document.createElement(\"" ++ hostName ++ "\"));")
          ]
      }
javaScriptTypeHandlerHostContainer _ = Nothing