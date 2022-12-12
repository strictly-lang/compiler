module Prelude.Javascript.Types.Record where

import Data.List (find, intercalate)
import Parser.Types (ASTExpression' (ASTExpressionRecord), ASTLeftHandSide (ASTLeftHandSideHole, ASTLeftHandSideRecord, ASTLeftHandSideVariable), ASTTypeDeclaration (ASTTypeDeclarationRecord))
import Prelude.Javascript.ReferenceTypeHandler (referenceTypeHandlerFactory)
import Prelude.Javascript.Types
import Prelude.Javascript.Util
import TypeChecker.Main (findTypehandler)
import TypeChecker.Types (TypeValue (TypeValueByLiteral, TypeValueByReference))

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
          { destructure = \renderContext leftHandSide -> do
              case leftHandSide of
                ASTLeftHandSideHole -> do return []
                ASTLeftHandSideVariable variableName -> do
                  originCode <- getExpressionCode <$> getExpressionContainer reference renderContext

                  return [((variableName, originCode, result), [])]
                ASTLeftHandSideRecord leftHandSideRecords -> do
                  result <-
                    mapM
                      ( \(leftHandSideRecordName, maybeNestedLeftHandSide) -> do
                          originCode <- getExpressionCode <$> getExpressionContainer reference renderContext
                          let propertyType = find (\(propertyName, _) -> leftHandSideRecordName == propertyName) recordTypes

                          case propertyType of
                            Just (_, propertyType) -> do
                              let nestedCode = originCode ++ [Ln ".", Ln leftHandSideRecordName]
                              let (Just typeHandler) = findTypehandler typeHandlerContext (Just propertyType) (TypeValueByReference (referenceTypeHandlerFactory nestedCode))
                              return [((leftHandSideRecordName, nestedCode, typeHandler), [])]
                            Nothing ->
                              error ("could not find property" ++ leftHandSideRecordName)
                      )
                      leftHandSideRecords

                  return (concat result)
                leftHandSide -> error ("such lefthandside is not implemented on record " ++ show leftHandSide),
            getDom = \renderContext -> do
              error "a record is not mountable inside the dom",
            getExpressionContainer = \renderContext -> do
              referenceExpressionCode <- getExpressionContainer reference renderContext
              return
                ( JavaScriptExpressionResult
                    { getExpressionCode =
                        getExpressionCode referenceExpressionCode
                    }
                )
          }
   in Just result
javaScriptTypeHandlerRecordContainer typeHandlerContext _ _ = Nothing