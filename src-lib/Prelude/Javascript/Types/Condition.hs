module Prelude.Javascript.Types.Condition where

import Parser.Types (ASTExpression' (ASTExpressionCondition), ASTLeftHandSide (ASTLeftHandSideHole, ASTLeftHandSideVariable), ASTTypeDeclaration (ASTTypeDeclarationAlgebraicDataType))
import Prelude.Javascript.Types
import TypeChecker.Types (TypeValue (TypeValueByLiteral, TypeValueByReference))

javaScriptTypeHandlerConditionContainer :: TypeHandlerContainer
javaScriptTypeHandlerConditionContainer typeHandlerContext typeDefinition ((TypeValueByLiteral (ASTExpressionCondition conditionExpression thenStatements elseStatements)) : restTypeValues) =
  let result =
        JavaScriptTypeHandler
          { destructure = \renderContext leftHandSide -> do
              error "no destructure available for Void",
            getDom = \renderContext -> do
              return
                JavaScriptDomResult
                  { create = [],
                    update = [],
                    dealloc = [],
                    delete = [],
                    siblings = []
                  },
            getExpressionContainer = \_ -> error "not yet implemented",
            call = \_ -> error "no functioncall available for Condition"
          }
   in Just result
javaScriptTypeHandlerConditionContainer typeHandlerContext _ _ = Nothing