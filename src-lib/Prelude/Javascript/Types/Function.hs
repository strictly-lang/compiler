module Prelude.Javascript.Types.Function where

import Parser.Types (ASTExpression' (ASTExpressionFunctionDeclaration), ASTLeftHandSide (ASTLeftHandSideVariable), ASTTypeDeclaration (ASTTypeDeclarationFunction, ASTTypeDeclarationGeneric))
import Prelude.Javascript.Types
import TypeChecker.Main (getStackEntries, typecheck)
import TypeChecker.Types

typeHandlerContainerFunction :: TypeHandlerContainer JavascriptTypeHandler
typeHandlerContainerFunction (ASTTypeDeclarationFunction parameter returnType) functionExpressions =
  let self =
        ( JavascriptTypeHandler
            { Prelude.Javascript.Types.properties = const [],
              Prelude.Javascript.Types.call = \typehandlerContainers stack parametersTypeHandlers ->
                Just
                  ( case (returnType, functionExpressions) of
                      (ASTTypeDeclarationGeneric _, (ASTExpressionFunctionDeclaration parametersVariables body) : functionOverloads) ->
                        let stack' = concat (reverse (zipWith getStackEntries parametersTypeHandlers parametersVariables)) ++ stack
                            (Right typedBody) = typecheck typehandlerContainers stack' body
                            (TypedExpression typeHandler) = last typedBody
                         in typeHandler
                      _ ->
                        error "mop"
                  ),
              Prelude.Javascript.Types.getDom = error "not implemented",
              Prelude.Javascript.Types.getCode = error "not implemented"
            }
        )
   in Just self
typeHandlerContainerFunction typeDefinition expressions = Nothing
