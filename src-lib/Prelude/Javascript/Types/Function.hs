module Prelude.Javascript.Types.Function where

import Parser.Types (ASTExpression' (ASTExpressionFunctionDeclaration))
import Prelude.Javascript.Types
import TypeChecker.Types (TypeHandlerContainer)

typeHandlerContainerFunction :: TypeHandlerContainer JavascriptTypeHandler
typeHandlerContainerFunction typeDefinition ([ASTExpressionFunctionDeclaration parameterLeftHandSide body] : overloads) =
  Just (JavascriptTypeHandler {})
typeHandlerContainerFunction typeDefinition expressions = Nothing