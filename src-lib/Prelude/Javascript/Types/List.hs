module Prelude.Javascript.Types.List where

import Parser.Types (ASTLeftHandSide (ASTLeftHandSideHole, ASTLeftHandSideRecord, ASTLeftHandSideVariable), ASTTypeDeclaration (ASTTypeDeclarationAlgebraicDataType))
import Prelude.Javascript.Types
import TypeChecker.Main (findTypehandler)
import TypeChecker.Types (TypeValue (TypeValueByReference))

javaScriptTypeHandlerListContainer :: TypeHandlerContainer
javaScriptTypeHandlerListContainer typeHandlerContext (Just (ASTTypeDeclarationAlgebraicDataType "List" [entityType])) ((TypeValueByReference referenceExpressionResult) : restTypeValues) =
  let result =
        JavaScriptTypeHandler
          { destructure = \renderContext leftHandSide -> do
              let originCode = getExpressionCode referenceExpressionResult

              case leftHandSide of
                ASTLeftHandSideHole -> do return []
                ASTLeftHandSideVariable variableName -> do
                  return [((variableName, selfDependency referenceExpressionResult, result), [])]
                ASTLeftHandSideRecord leftHandSideRecords -> do
                  result <-
                    mapM
                      ( \(leftHandSideRecordName, maybeNestedLeftHandSide) -> do
                          let nestedSelfDependency = (\selfDependency -> selfDependency ++ [DotNotation leftHandSideRecordName]) <$> selfDependency referenceExpressionResult
                           in case leftHandSideRecordName of
                                "map" -> do
                                  return [((leftHandSideRecordName, nestedSelfDependency, listMapTypeHandler), [])]
                                "zip" -> do
                                  error "zip is not yet implemented"
                                "length" -> do
                                  let nestedCode = originCode ++ [Ln ".", Ln leftHandSideRecordName]
                                  let (Just typeHandler) =
                                        findTypehandler
                                          typeHandlerContext
                                          (Just (ASTTypeDeclarationAlgebraicDataType "Number" []))
                                          [ TypeValueByReference
                                              ( JavaScriptExpressionResult
                                                  { getExpressionCode = nestedCode,
                                                    selfDependency = nestedSelfDependency,
                                                    extraDependencies = extraDependencies referenceExpressionResult
                                                  }
                                              )
                                          ]
                                  return [((leftHandSideRecordName, nestedSelfDependency, typeHandler), [])]
                                _ ->
                                  error ("could not find property " ++ leftHandSideRecordName)
                      )
                      leftHandSideRecords

                  return (reverse (concat result))
                leftHandSide -> error ("such lefthandside is not implemented on list " ++ show leftHandSide),
            getDom = \_ -> error "no dom available for List",
            getExpressionContainer = \_ -> do return referenceExpressionResult,
            call = \_ -> error "no functioncall available for List"
          }
   in Just result
javaScriptTypeHandlerListContainer typeHandlerContext _ _ = Nothing

listMapTypeHandler :: JavaScriptTypeHandler
listMapTypeHandler =
  JavaScriptTypeHandler
    { destructure = \renderContext leftHandSide -> do
        error ("no destructure available for list-map declaration " ++ show leftHandSide),
      getDom = \_ -> error "no dom available for list-map declaration",
      getExpressionContainer = \_ -> do
        error "no getExpressionContainer available for list-map declaration",
      call = \_ parameters -> do
        return
          ( JavaScriptTypeHandler
              { destructure = \renderContext leftHandSide -> do
                  error "no destructure available for list-map",
                getDom = \renderContext ->
                  return
                    ( JavaScriptDomResult
                        { create =
                            [],
                          update = [],
                          dealloc = [],
                          delete = [],
                          siblings = []
                        }
                    ),
                getExpressionContainer = \_ -> do
                  error "no getExpressionContainer available for list-map",
                call = \_ -> error "no functioncall available for list-map"
              }
          )
    }