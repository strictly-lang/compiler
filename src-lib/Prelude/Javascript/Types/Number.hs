module Prelude.Javascript.Types.Number where

import Parser.Types (ASTExpression' (ASTExpressionAlgebraicDataType, ASTExpressionNumber), ASTLeftHandSide (ASTLeftHandSideHole, ASTLeftHandSideRecord, ASTLeftHandSideVariable), ASTTypeDeclaration (ASTTypeDeclarationAlgebraicDataType))
import Prelude.Javascript.Types
import Prelude.Javascript.Util (nestedExpression)
import TypeChecker.Main (findTypehandler)
import TypeChecker.Types (TypeValue (TypeValueByLiteral, TypeValueByReference))

javaScriptTypeHandlerNumberContainer :: TypeHandlerContainer
javaScriptTypeHandlerNumberContainer typeHandlerContext (Just (ASTTypeDeclarationAlgebraicDataType "Number" [])) ((TypeValueByReference referenceExpressionResult) : restTypeValues) =
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
                                "equal" -> do
                                  return
                                    [ ( ( leftHandSideRecordName,
                                          nestedSelfDependency,
                                          JavaScriptTypeHandler
                                            { destructure = \renderContext leftHandSide -> do
                                                error ("no destructure available for list-map declaration " ++ show leftHandSide),
                                              getDom = \_ -> error "no dom available for list-map declaration",
                                              getExpressionContainer = \_ -> do
                                                error "no getExpressionContainer available for list-map declaration",
                                              call = \_ [bExpression] -> do
                                                bExpressionTypeHandler <- nestedExpression renderContext (Just (ASTTypeDeclarationAlgebraicDataType "Number" [])) [bExpression]
                                                bExpression <- getExpressionContainer bExpressionTypeHandler renderContext
                                                let nestedCode = originCode ++ [Ln " == "] ++ getExpressionCode bExpression
                                                let (Just typeHandler) =
                                                      findTypehandler
                                                        typeHandlerContext
                                                        (Just (ASTTypeDeclarationAlgebraicDataType "Boolean" []))
                                                        [ TypeValueByReference
                                                            ( JavaScriptExpressionResult
                                                                { getExpressionCode = nestedCode,
                                                                  selfDependency = nestedSelfDependency,
                                                                  extraDependencies = extraDependencies referenceExpressionResult
                                                                }
                                                            )
                                                        ]
                                                return typeHandler
                                            }
                                        ),
                                        []
                                      )
                                    ]
                                _ ->
                                  error ("could not find property " ++ leftHandSideRecordName)
                      )
                      leftHandSideRecords

                  return (reverse (concat result))
                leftHandSide -> error ("such lefthandside is not implemented on list " ++ show leftHandSide),
            getDom = \_ -> error "no dom available for Number",
            getExpressionContainer = \_ -> do return referenceExpressionResult,
            call = \_ -> error "no functioncall available for Number"
          }
   in Just result
javaScriptTypeHandlerNumberContainer typeHandlerContext _ ((TypeValueByLiteral (ASTExpressionNumber number)) : restTypeValues) =
  Just
    JavaScriptTypeHandler
      { destructure = error "no property access implemented", -- TODO
        Prelude.Javascript.Types.getDom = \renderContext -> do
          error "no dom available for number",
        getExpressionContainer = \_ -> do
          return
            ( JavaScriptExpressionResult
                { getExpressionCode = [Ln (show number)],
                  selfDependency = Nothing,
                  extraDependencies = []
                }
            ),
        call = \_ -> error "no functioncall available for number"
      }
javaScriptTypeHandlerNumberContainer typeHandlerContext _ _ = Nothing