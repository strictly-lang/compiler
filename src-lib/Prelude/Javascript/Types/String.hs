module Prelude.Javascript.Types.String where

import Data.List (intercalate)
import Parser.Types (ASTExpression' (ASTExpressionString), ASTString (ASTStringDynamic, ASTStringStatic))
import Prelude.Javascript.Types
import Prelude.Javascript.Util
import TypeChecker.Types (TypeValue (TypeValueByLiteral))

javaScriptTypeHandlerStringContainer :: TypeHandlerContainer
javaScriptTypeHandlerStringContainer typeHandlerContext _ (TypeValueByLiteral (ASTExpressionString astStrings)) =
  let expressionCode renderContext _ = do
        result <-
          mapM
            ( \stringPart ->
                do
                  case stringPart of
                    ASTStringStatic value -> do return [Ln ("\"" ++ value ++ "\"")]
                    ASTStringDynamic expression -> do
                      typeHandler <- nestedExpression renderContext expression
                      Prelude.Javascript.Types.expression <$> getExpression typeHandler renderContext []
            )
            astStrings
        return
          ( JavaScriptExpressionResult
              { expression = intercalate [Ln " + "] result
              }
          )
   in Just
        JavaScriptTypeHandler
          { destructure = error "no property access implemented",
            getDom = \renderContext -> do
              exprId <- getGetFreshExprId
              let text = "text" ++ show exprId
              result <- expressionCode renderContext []

              return
                ( JavaScriptDomResult
                    { create =
                        [Ln ("const " ++ text ++ " = document.createTextNode(")]
                          ++ expression result
                          ++ [ Ln ");",
                               Br,
                               Ln (runParent renderContext ++ ".append(" ++ text ++ ");"),
                               Br
                             ],
                      update = [],
                      dealloc = [],
                      delete = []
                    }
                ),
            getExpression =
              expressionCode
          }
javaScriptTypeHandlerStringContainer typeHandlerContext _ _ = Nothing