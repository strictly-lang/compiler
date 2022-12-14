module Prelude.Javascript.Types.Host where

import Data.List (intercalate)
import Data.Maybe (maybeToList)
import Parser.Types (ASTExpression' (ASTExpressionHost), ASTRecordValue (RecordExpression))
import Prelude.Javascript.Types
import Prelude.Javascript.Util
import TypeChecker.Types (TypeValue (TypeValueByLiteral))

javaScriptTypeHandlerHostContainer :: TypeHandlerContainer
javaScriptTypeHandlerHostContainer typeHandlerContext _ (TypeValueByLiteral (ASTExpressionHost hostName (attributes, []) children)) =
  Just
    JavaScriptTypeHandler
      { destructure = error "no property access implemented",
        getDom = \renderContext -> do
          exprId <- getGetFreshExprId
          let element = runScope renderContext ++ [DotNotation ("element" ++ show exprId)]

          attributesExpressions <-
            mapM
              ( \(attributeName, RecordExpression Nothing expression) -> do
                  typeHandler <- nestedExpression renderContext expression
                  result <- getExpressionContainer typeHandler renderContext

                  return (attributeName, result)
              )
              attributes

          nestedResult <-
            render
              ( JavaScriptRenderContext
                  { runParent = element,
                    runTypes = runTypes renderContext,
                    runStack = runStack renderContext,
                    runScope = runScope renderContext
                  }
              )
              children

          return
            ( JavaScriptDomResult
                { create =
                    propertyToCode element
                      ++ [ Ln (" = document.createElement(\"" ++ hostName ++ "\");"),
                           Br
                         ]
                      ++ concatMap
                        ( \(attributeName, attributeExpressions) ->
                            propertyToCode element ++ [Ln (".setAttribute(\"" ++ attributeName ++ "\", ")] ++ getExpressionCode attributeExpressions ++ [Ln ");", Br]
                        )
                        attributesExpressions
                      ++ propertyToCode (runParent renderContext)
                      ++ [ Ln ".append("
                         ]
                      ++ propertyToCode element
                      ++ [ Ln ");",
                           Br,
                           Br
                         ]
                      ++ create nestedResult,
                  update =
                    concat
                      [ map
                          ( \dependency ->
                              ( dependency,
                                propertyToCode element ++ [Ln (".setAttribute(\"" ++ attributeName ++ "\", ")] ++ getExpressionCode attributeExpressionResult ++ [Ln ");", Br]
                              )
                          )
                          (maybeToList (selfDependency attributeExpressionResult) ++ extraDependencies attributeExpressionResult)
                        | (attributeName, attributeExpressionResult) <- attributesExpressions
                      ]
                      ++ update nestedResult,
                  dealloc = [] ++ dealloc nestedResult,
                  delete = [] ++ delete nestedResult
                }
            ),
        getExpressionContainer = error "no expression container available for host"
      }
javaScriptTypeHandlerHostContainer typeHandlerContext _ _ = Nothing