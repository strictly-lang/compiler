module Prelude.Javascript.Types.Void where

import Parser.Types (ASTLeftHandSide (ASTLeftHandSideHole, ASTLeftHandSideVariable), ASTTypeDeclaration (ASTTypeDeclarationAlgebraicDataType))
import Prelude.Javascript.Types
import TypeChecker.Types (TypeValue (TypeValueByReference))

javaScriptTypeHandlerVoidContainer :: TypeHandlerContainer
javaScriptTypeHandlerVoidContainer typeHandlerContext (Just (ASTTypeDeclarationAlgebraicDataType "Void" [])) ((TypeValueByReference referenceExpressionResult) : restTypeValues) =
  let result =
        JavaScriptTypeHandler
          { destructure = \renderContext leftHandSide -> do
              case leftHandSide of
                ASTLeftHandSideHole -> do return []
                ASTLeftHandSideVariable variableName -> do
                  let originCode = getExpressionCode referenceExpressionResult

                  return [((variableName, selfDependency referenceExpressionResult, result), [])]
                leftHandSide -> error ("such lefthandside is not implemented on record " ++ show leftHandSide),
            getDom = \_ -> error "no functioncall available for Void",
            getExpressionContainer = \_ -> do return referenceExpressionResult,
            call = \_ -> error "no functioncall available for Void"
          }
   in Just result
javaScriptTypeHandlerVoidContainer typeHandlerContext _ _ = Nothing