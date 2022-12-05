module Prelude.Javascript.Types.String where

import Data.List (intercalate)
import Parser.Types (ASTExpression' (ASTExpressionString), ASTString (ASTStringStatic))
import Prelude.Javascript.Types
import Prelude.Javascript.Util

javaScriptTypeHandlerStringContainer :: TypeHandlerContainer
javaScriptTypeHandlerStringContainer typeHandlerContext _ ((ASTExpressionString astStrings)) =
  Just
    JavaScriptTypeHandler
      { destructure = error "no property access implemented",
        getDom = \renderContext -> do
          exprId <- getGetFreshExprId
          let text = "text" ++ show exprId

          return
            ( JavaScriptDomResult
                { create =
                    [ Ln ("const " ++ text ++ " = document.createTextNode(" ++ intercalate " + " (['"' : astString ++ ['"'] | ASTStringStatic astString <- astStrings]) ++ ");"),
                      Br,
                      Ln (runParent renderContext ++ ".append(" ++ text ++ ");"),
                      Br
                    ],
                  update = [],
                  dealloc = [],
                  delete = []
                }
            )
      }
javaScriptTypeHandlerStringContainer typeHandlerContext _ _ = Nothing