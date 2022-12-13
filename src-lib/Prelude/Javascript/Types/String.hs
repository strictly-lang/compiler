module Prelude.Javascript.Types.String where

import Data.List (intercalate)
import Data.Maybe (mapMaybe, maybeToList)
import Parser.Types (ASTExpression' (ASTExpressionString), ASTString (ASTStringDynamic, ASTStringStatic), ASTTypeDeclaration (ASTTypeDeclarationAlgebraicDataType))
import Prelude.Javascript.Types
import Prelude.Javascript.Util
import TypeChecker.Types (TypeValue (TypeValueByLiteral, TypeValueByReference))

getDom :: JavaScriptRenderContext -> JavaScriptExpressionResult -> AppStateMonad JavaScriptDomResult
getDom renderContext expressionResult = do
  exprId <- getGetFreshExprId
  let text = runScope renderContext ++ [DotNotation ("text" ++ show exprId)]

  return
    ( JavaScriptDomResult
        { create =
            propertyToCode text
              ++ [Ln " = document.createTextNode("]
              ++ getExpressionCode expressionResult
              ++ [ Ln ");",
                   Br,
                   Ln (runParent renderContext ++ ".append(")
                 ]
              ++ propertyToCode text
              ++ [ Ln ")",
                   Br
                 ],
          update =
            map
              ( \dependency ->
                  ( dependency,
                    propertyToCode (text ++ [DotNotation "textContent"])
                      ++ [Ln " = "]
                      ++ getExpressionCode expressionResult
                  )
              )
              (maybeToList (selfDependency expressionResult) ++ extraDependencies expressionResult),
          dealloc = [],
          delete = []
        }
    )

javaScriptTypeHandlerStringContainer :: TypeHandlerContainer
javaScriptTypeHandlerStringContainer typeHandlerContext _ (TypeValueByLiteral (ASTExpressionString astStrings)) =
  let expressionCode astStrings renderContext = do
        result <-
          mapM
            ( \stringPart ->
                do
                  case stringPart of
                    ASTStringStatic value -> do
                      return
                        ( ( JavaScriptExpressionResult
                              { getExpressionCode = [Ln ("\"" ++ value ++ "\"")],
                                selfDependency = Nothing,
                                extraDependencies = []
                              }
                          )
                        )
                    ASTStringDynamic expression -> do
                      typeHandler <- nestedExpression renderContext expression
                      getExpressionContainer typeHandler renderContext
            )
            astStrings
        return
          ( JavaScriptExpressionResult
              { getExpressionCode = intercalate [Ln " + "] (map getExpressionCode result),
                selfDependency = Nothing,
                extraDependencies = mapMaybe selfDependency result ++ concatMap extraDependencies result
              }
          )
   in Just
        JavaScriptTypeHandler
          { destructure = error "no property access implemented",
            Prelude.Javascript.Types.getDom = \renderContext -> do
              result <- expressionCode astStrings renderContext
              Prelude.Javascript.Types.String.getDom renderContext result,
            getExpressionContainer =
              expressionCode astStrings
          }
javaScriptTypeHandlerStringContainer typeHandlerContext (Just (ASTTypeDeclarationAlgebraicDataType "String" [])) (TypeValueByReference referenceExpressionResult) =
  Just
    JavaScriptTypeHandler
      { destructure = error "no property access implemented",
        Prelude.Javascript.Types.getDom = \renderContext -> do
          Prelude.Javascript.Types.String.getDom renderContext referenceExpressionResult,
        getExpressionContainer = \_ -> do return referenceExpressionResult
      }
javaScriptTypeHandlerStringContainer typeHandlerContext _ _ = Nothing