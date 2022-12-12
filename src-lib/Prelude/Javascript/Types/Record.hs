module Prelude.Javascript.Types.Record where

import Data.List (intercalate)
import Parser.Types (ASTExpression' (ASTExpressionRecord), ASTLeftHandSide (ASTLeftHandSideHole, ASTLeftHandSideRecord, ASTLeftHandSideVariable), ASTTypeDeclaration (ASTTypeDeclarationRecord))
import Prelude.Javascript.Types
import Prelude.Javascript.Util
import TypeChecker.Types (Property (DotNotation), TypeValue (TypeValueByLiteral, TypeValueByReference))

javaScriptTypeHandlerRecordContainer :: TypeHandlerContainer
javaScriptTypeHandlerRecordContainer typeHandlerContext _ (TypeValueByLiteral (ASTExpressionRecord astRecords)) =
  Just
    JavaScriptTypeHandler
      { destructure = error "no property access implemented",
        getDom = \renderContext -> do
          error "a record is not mountable inside the dom"
      }
javaScriptTypeHandlerRecordContainer typeHandlerContext (Just (ASTTypeDeclarationRecord recordTypes)) (TypeValueByReference reference) =
  let result =
        JavaScriptTypeHandler
          { destructure = \leftHandSide -> do
              case leftHandSide of
                ASTLeftHandSideHole -> do return []
                ASTLeftHandSideVariable variableName -> do
                  return [((variableName, reference, result), [])]
                ASTLeftHandSideRecord leftHandSideRecords -> do
                  result <-
                    mapM
                      ( \(leftHandSideRecordName, maybeNestedLeftHandSide) -> do
                          return [((leftHandSideRecordName, reference ++ [DotNotation leftHandSideRecordName], result), [])]
                      )
                      leftHandSideRecords

                  return (concat result)
                leftHandSide -> error ("such lefthandside is not implemented on record " ++ show leftHandSide),
            getDom = \renderContext -> do
              error "a record is not mountable inside the dom"
          }
   in Just result
javaScriptTypeHandlerRecordContainer typeHandlerContext _ _ = Nothing