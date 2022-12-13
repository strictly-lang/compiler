module Prelude.Javascript.Types.Host where

import Parser.Types (ASTExpression' (ASTExpressionHost))
import Prelude.Javascript.Types
import Prelude.Javascript.Util
import TypeChecker.Types (TypeValue (TypeValueByLiteral))

javaScriptTypeHandlerHostContainer :: TypeHandlerContainer
javaScriptTypeHandlerHostContainer typeHandlerContext _ (TypeValueByLiteral (ASTExpressionHost hostName attributes children)) =
  Just
    JavaScriptTypeHandler
      { destructure = error "no property access implemented",
        getDom = \renderContext -> do
          exprId <- getGetFreshExprId
          let element = "element" ++ show exprId
          nestedResult <-
            render
              ( JavaScriptRenderContext
                  { runParent = element,
                    runTypes = runTypes renderContext,
                    runStack = runStack renderContext,
                    runScope = runScope renderContext
                  }
              )
              children

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
javaScriptTypeHandlerHostContainer typeHandlerContext _ _ = Nothing