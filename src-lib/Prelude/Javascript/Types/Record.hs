module Prelude.Javascript.Types.Record where

import Data.List (find, intercalate)
import Parser.Types (ASTExpression' (ASTExpressionRecord), ASTLeftHandSide (ASTLeftHandSideHole, ASTLeftHandSideRecord, ASTLeftHandSideVariable), ASTTypeDeclaration (ASTTypeDeclarationRecord))
import Prelude.Javascript.Types
import Prelude.Javascript.Util
import TypeChecker.Main (findTypehandler)
import TypeChecker.Types (TypeValue (TypeValueByLiteral, TypeValueByReference))

javaScriptTypeHandlerRecordContainer :: TypeHandlerContainer
javaScriptTypeHandlerRecordContainer typeHandlerContext _ (TypeValueByLiteral (ASTExpressionRecord astRecords) : restTypeValues) =
  Just
    JavaScriptTypeHandler
      { destructure = error "no property access implemented",
        getDom = \renderContext -> do
          error "a record is not mountable inside the dom",
        getExpressionContainer = \renderContext -> do
          error "expression container is not yet implemented"
      }
javaScriptTypeHandlerRecordContainer typeHandlerContext (Just (ASTTypeDeclarationRecord recordTypes)) ((TypeValueByReference referenceExpressionResult) : restTypeValues) =
  let result =
        JavaScriptTypeHandler
          { destructure = \renderContext leftHandSide -> do
              case leftHandSide of
                ASTLeftHandSideHole -> do return []
                ASTLeftHandSideVariable variableName -> do
                  let originCode = getExpressionCode referenceExpressionResult

                  return [((variableName, selfDependency referenceExpressionResult, result), [])]
                ASTLeftHandSideRecord leftHandSideRecords -> do
                  result <-
                    mapM
                      ( \(leftHandSideRecordName, maybeNestedLeftHandSide) -> do
                          let originCode = getExpressionCode referenceExpressionResult
                          let propertyType = find (\(propertyName, _) -> leftHandSideRecordName == propertyName) recordTypes

                          case propertyType of
                            Just (_, propertyType) -> do
                              let nestedCode = originCode ++ [Ln ".", Ln leftHandSideRecordName]
                              let nestedSelfDependency = (\selfDependency -> selfDependency ++ [DotNotation leftHandSideRecordName]) <$> selfDependency referenceExpressionResult

                              let (Just typeHandler) =
                                    findTypehandler
                                      typeHandlerContext
                                      (Just propertyType)
                                      [ TypeValueByReference
                                          ( JavaScriptExpressionResult
                                              { getExpressionCode = nestedCode,
                                                selfDependency = nestedSelfDependency,
                                                extraDependencies = extraDependencies referenceExpressionResult
                                              }
                                          )
                                      ]
                              return [((leftHandSideRecordName, nestedSelfDependency, typeHandler), [])]
                            Nothing ->
                              error ("could not find property" ++ leftHandSideRecordName)
                      )
                      leftHandSideRecords

                  return (reverse (concat result))
                leftHandSide -> error ("such lefthandside is not implemented on record " ++ show leftHandSide),
            getDom = \renderContext -> do
              error "a record is not mountable inside the dom",
            getExpressionContainer = \_ -> do return referenceExpressionResult
          }
   in Just result
javaScriptTypeHandlerRecordContainer typeHandlerContext _ _ = Nothing