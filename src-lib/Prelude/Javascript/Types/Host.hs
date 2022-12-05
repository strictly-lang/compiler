module Prelude.Javascript.Types.Host where

import Parser.Types (ASTExpression' (ASTExpressionHost))
import Prelude.Javascript.Types
import Prelude.Javascript.Util

javaScriptTypeHandlerHostContainer :: TypeHandlerContainer
javaScriptTypeHandlerHostContainer typeHandlerContext [ASTExpressionHost hostName attributes children] =
  Just
    JavaScriptTypeHandler
      { getProperty = error "no property access implemented",
        getDom = \renderContext -> do
          exprId <- getGetFreshExprId
          let element = "element" ++ show exprId
          nestedResult <- render (JavaScriptRenderContext {runParent = element, runTypes = runTypes renderContext}) children

          return
            ( JavaScriptDomResult
                { create =
                    [ Ln ("const " ++ element ++ " = document.createElement(\"" ++ hostName ++ "\");"),
                      Br,
                      Ln (runParent renderContext ++ ".append(" ++ element ++ ");"),
                      Br
                    ]
                      ++ create nestedResult,
                  update = [] ++ update nestedResult,
                  dealloc = [] ++ dealloc nestedResult,
                  delete = [] ++ delete nestedResult
                }
            )
      }
javaScriptTypeHandlerHostContainer types _ = Nothing