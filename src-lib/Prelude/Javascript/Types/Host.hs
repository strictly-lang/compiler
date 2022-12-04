module Prelude.Javascript.Types.Host where

import Parser.Types (ASTExpression, ASTExpression' (ASTExpressionHost, ASTExpressionString), ASTString (ASTStringStatic))
import Prelude.Javascript.Types
import Prelude.Javascript.Util
import TypeChecker.Types (TypeHandlerContext)

javaScriptTypeHandlerHostContainer :: TypeHandlerContext JavaScriptTypeHandler -> ASTExpression -> Maybe JavaScriptTypeHandler
javaScriptTypeHandlerHostContainer typeHandlerContext [ASTExpressionHost hostName attributes children] =
  Just
    JavaScriptTypeHandler
      { getProperty = error "no property access implemented",
        getDom = \renderContext ->
          let element = "element"
           in [ Ln ("var element = document.createElement(\"" ++ hostName ++ "\");"),
                Br,
                Ln (runParent renderContext ++ ".append(" ++ element ++ ");"),
                Br
              ]
                ++ render (JavaScriptRenderContext {runParent = element, runTypes = runTypes renderContext}) children
      }
javaScriptTypeHandlerHostContainer types _ = Nothing