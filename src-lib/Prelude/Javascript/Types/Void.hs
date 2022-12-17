module Prelude.Javascript.Types.Void where

import Parser.Types (ASTLeftHandSide (ASTLeftHandSideHole, ASTLeftHandSideVariable), ASTTypeDeclaration (ASTTypeDeclarationAlgebraicDataType))
import Prelude.Javascript.Types
import TypeChecker.Types (TypeValue (TypeValueByReference))

javaScriptTypeHandlerVoidContainer :: TypeHandlerContainer
javaScriptTypeHandlerVoidContainer typeHandlerContext (Just (ASTTypeDeclarationAlgebraicDataType "Void" [])) ((TypeValueByReference referenceExpressionResult) : restTypeValues) =
  let result =
        JavaScriptTypeHandler
          { destructure = \renderContext leftHandSide -> do
              error "no destructure available for Void",
            getDom = \_ -> error "no dom available for Void",
            getExpressionContainer = \_ -> do return referenceExpressionResult,
            call = \_ -> error "no functioncall available for Void"
          }
   in Just result
javaScriptTypeHandlerVoidContainer typeHandlerContext _ _ = Nothing