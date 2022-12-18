module Prelude.Javascript.Types.List where

import Parser.Types (ASTLeftHandSide (ASTLeftHandSideHole, ASTLeftHandSideVariable), ASTTypeDeclaration (ASTTypeDeclarationAlgebraicDataType))
import Prelude.Javascript.Types
import TypeChecker.Types (TypeValue (TypeValueByReference))

javaScriptTypeHandlerListContainer :: TypeHandlerContainer
javaScriptTypeHandlerListContainer typeHandlerContext (Just (ASTTypeDeclarationAlgebraicDataType "List" [entityType])) ((TypeValueByReference referenceExpressionResult) : restTypeValues) =
  let result =
        JavaScriptTypeHandler
          { destructure = \renderContext leftHandSide -> do
              error "no destructure available for List",
            getDom = \_ -> error "no dom available for List",
            getExpressionContainer = \_ -> do return referenceExpressionResult,
            call = \_ -> error "no functioncall available for List"
          }
   in Just result
javaScriptTypeHandlerListContainer typeHandlerContext _ _ = Nothing