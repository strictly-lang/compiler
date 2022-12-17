module Prelude.Javascript.Types.Host where

import Data.List (intercalate, isPrefixOf)
import Data.Maybe (maybeToList)
import Parser.Types (ASTExpression' (ASTExpressionHost))
import Prelude.Javascript.Types
import Prelude.Javascript.Util
import TypeChecker.Types (TypeValue (TypeValueByLiteral))

eventPrefix = "on"

javaScriptTypeHandlerHostContainer :: TypeHandlerContainer
javaScriptTypeHandlerHostContainer typeHandlerContext _ (TypeValueByLiteral (ASTExpressionHost hostName (attributes, []) children) : restTypeValues) =
  Just
    JavaScriptTypeHandler
      { destructure = error "no property access implemented",
        getDom = \renderContext -> do
          exprId <- getGetFreshExprId
          let element = runScope renderContext ++ [DotNotation ("element" ++ show exprId)]

          options <-
            mapM
              ( \(optionName, (typedefinition, expressions)) -> do
                  typeHandler <- nestedExpression renderContext typedefinition (map snd expressions)
                  result <- getExpressionContainer typeHandler renderContext

                  return (optionName, result)
              )
              attributes

          nestedResult <-
            render
              ( JavaScriptRenderContext
                  { runParent = element,
                    runTypes = runTypes renderContext,
                    runStack = runStack renderContext,
                    runScope = runScope renderContext,
                    runSiblings = []
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
                        ( \(optionName, attributeExpressions) ->
                            propertyToCode element
                              ++ [ if eventPrefix `isPrefixOf` optionName
                                     then Ln (".addEventListener(\"" ++ drop (length eventPrefix) optionName ++ "\", ")
                                     else Ln (".setAttribute(\"" ++ optionName ++ "\", ")
                                 ]
                              ++ getExpressionCode attributeExpressions
                              ++ [Ln ");", Br]
                        )
                        options
                      ++ appendElement renderContext element
                      ++ [Br]
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
                        | (attributeName, attributeExpressionResult) <- options,
                          not (eventPrefix `isPrefixOf` attributeName)
                      ]
                      ++ update nestedResult,
                  dealloc = [] ++ dealloc nestedResult,
                  delete = propertyToCode element ++ [Ln ".remove();", Br],
                  siblings = [SiblingAlways element]
                }
            ),
        getExpressionContainer = error "no expression container available for host",
        call = \_ -> error "no functioncall available for host"
      }
javaScriptTypeHandlerHostContainer typeHandlerContext _ _ = Nothing