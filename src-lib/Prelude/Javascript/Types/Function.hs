module Prelude.Javascript.Types.Function where

import Parser.Types (ASTExpression' (ASTExpressionFunctionDeclaration), ASTTypeDeclaration (ASTTypeDeclarationFunction))
import Prelude.Javascript.Types
import TypeChecker.Types (TypeHandlerContainer)

typeHandlerContainerFunction :: TypeHandlerContainer JavascriptTypeHandler
typeHandlerContainerFunction (ASTTypeDeclarationFunction parameter returnType) =
  Just
    ( JavascriptTypeHandler
        { destructure = const Nothing,
          call = \parameters -> Nothing
        }
    )
typeHandlerContainerFunction typeDefinition = Nothing