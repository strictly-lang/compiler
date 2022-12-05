module Prelude.Javascript.Types.String where

import Data.List (intercalate)
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
          exprId <- getGetFreshExprId
          let text = "text" ++ show exprId
          return
            [ Ln ("const " ++ text ++ " = document.createTextNode(" ++ intercalate " + " (['"' : astString ++ ['"'] | ASTStringStatic astString <- astStrings]) ++ ");"),
              Br,
              Ln (runParent renderContext ++ ".append(" ++ text ++ ");")
            ]
      }
javaScriptTypeHandlerStringContainer types _ = Nothing