module Prelude.Javascript.Types.String where

import Data.List (intercalate)
import Parser.Types (ASTExpression' (ASTExpressionString), ASTString (ASTStringDynamic, ASTStringStatic), ASTTypeDeclaration (ASTTypeDeclarationAlgebraicDataType))
import Prelude.Javascript.Types
import Prelude.Javascript.Util
import TypeChecker.Types (TypeValue (TypeValueByLiteral, TypeValueByReference))

expressionCode :: [ASTString] -> JavaScriptRenderContext -> AppStateMonad JavaScriptExpressionResult
expressionCode astStrings renderContext = do
  result <-
    mapM
      ( \stringPart ->
          do
            case stringPart of
              ASTStringStatic value -> do return [Ln ("\"" ++ value ++ "\"")]
              ASTStringDynamic expression -> do
                typeHandler <- nestedExpression renderContext expression
                getExpressionCode <$> getExpressionContainer typeHandler renderContext
      )
      astStrings
  return
    ( JavaScriptExpressionResult
        { getExpressionCode = intercalate [Ln " + "] result
        }
    )

getDom :: JavaScriptRenderContext -> [Code] -> AppStateMonad JavaScriptDomResult
getDom renderContext code = do
  exprId <- getGetFreshExprId
  let text = "text" ++ show exprId

  return
    ( JavaScriptDomResult
        { create =
            [Ln ("const " ++ text ++ " = document.createTextNode(")]
              ++ code
              ++ [ Ln ");",
                   Br,
                   Ln (runParent renderContext ++ ".append(" ++ text ++ ");"),
                   Br
                 ],
          update = [],
          dealloc = [],
          delete = []
        }
    )

javaScriptTypeHandlerStringContainer :: TypeHandlerContainer
javaScriptTypeHandlerStringContainer typeHandlerContext _ (TypeValueByLiteral (ASTExpressionString astStrings)) =
  Just
    JavaScriptTypeHandler
      { destructure = error "no property access implemented",
        Prelude.Javascript.Types.getDom = \renderContext -> do
          result <- expressionCode astStrings renderContext
          Prelude.Javascript.Types.String.getDom renderContext (getExpressionCode result),
        getExpressionContainer =
          expressionCode astStrings
      }
javaScriptTypeHandlerStringContainer typeHandlerContext (Just (ASTTypeDeclarationAlgebraicDataType "String" [])) (TypeValueByReference referenceExpressionResult) =
  Just
    JavaScriptTypeHandler
      { destructure = error "no property access implemented",
        Prelude.Javascript.Types.getDom = \renderContext -> do
          Prelude.Javascript.Types.String.getDom renderContext (getExpressionCode referenceExpressionResult),
        getExpressionContainer = \_ -> do return referenceExpressionResult
      }
javaScriptTypeHandlerStringContainer typeHandlerContext _ _ = Nothing