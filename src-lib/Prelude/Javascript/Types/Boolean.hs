module Prelude.Javascript.Types.Boolean where

import Parser.Types (ASTLeftHandSide (ASTLeftHandSideHole, ASTLeftHandSideVariable), ASTTypeDeclaration (ASTTypeDeclarationAlgebraicDataType))
import Prelude.Javascript.Types
import TypeChecker.Types (TypeValue (TypeValueByReference))

javaScriptTypeHandlerBooleanContainer :: TypeHandlerContainer
javaScriptTypeHandlerBooleanContainer typeHandlerContext (Just (ASTTypeDeclarationAlgebraicDataType "Boolean" [])) ((TypeValueByReference referenceExpressionResult) : restTypeValues) =
  let result =
        JavaScriptTypeHandler
          { destructure = \renderContext leftHandSide -> do
              error "no destructure available for Boolean",
            getDom = \_ -> error "no dom available for Boolean",
            getExpressionContainer = \_ -> do return referenceExpressionResult,
            call = \_ -> error "no functioncall available for Boolean"
          }
   in Just result
javaScriptTypeHandlerBooleanContainer typeHandlerContext _ _ = Nothing