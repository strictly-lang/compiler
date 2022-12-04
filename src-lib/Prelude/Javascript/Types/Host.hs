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
        getDom = \renderContext -> do
          exprId <- getGetFreshExprId
          let element = "element" ++ show exprId
          nestedResult <- render (JavaScriptRenderContext {runParent = element, runTypes = runTypes renderContext}) children
          return
            ( [ Ln ("const " ++ element ++ " = document.createElement(\"" ++ hostName ++ "\");"),
                Br,
                Ln (runParent renderContext ++ ".append(" ++ element ++ ");"),
                Br
              ]
                ++ nestedResult
            )
      }
javaScriptTypeHandlerHostContainer types _ = Nothing