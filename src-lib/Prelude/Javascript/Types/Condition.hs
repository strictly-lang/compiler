module Prelude.Javascript.Types.Condition where

import Data.Maybe (maybeToList)
import Parser.Types (ASTExpression' (ASTExpressionCondition), ASTLeftHandSide (ASTLeftHandSideHole, ASTLeftHandSideVariable), ASTTypeDeclaration (ASTTypeDeclarationAlgebraicDataType))
import Prelude.Javascript.Types
import Prelude.Javascript.Util (getGetFreshExprId, nestedExpression, propertyToCode, render)
import TypeChecker.Types (TypeValue (TypeValueByLiteral, TypeValueByReference))

javaScriptTypeHandlerConditionContainer :: TypeHandlerContainer
javaScriptTypeHandlerConditionContainer typeHandlerContext typeDefinition ((TypeValueByLiteral (ASTExpressionCondition conditionExpression thenStatements elseStatements)) : restTypeValues) =
  let result =
        JavaScriptTypeHandler
          { destructure = \renderContext leftHandSide -> do
              error "no destructure available for Void",
            getDom = \renderContext -> do
              exprId <- getGetFreshExprId
              let createCallback = runScope renderContext ++ [DotNotation ("createCallback" ++ show exprId)]
              let createParameter = [DotNotation ("value" ++ show exprId)]
              let deleteCallback = runScope renderContext ++ [DotNotation ("deleteCallback" ++ show exprId)]
              let deleteParameter = [DotNotation ("value" ++ show exprId)]
              let conditionCache = runScope renderContext ++ [DotNotation ("conditionCache" ++ show exprId)]
              let conditionCompareCache = runScope renderContext ++ [DotNotation ("conditionCompareCache" ++ show exprId)]
              conditionTypeHandler <- nestedExpression renderContext (Just (ASTTypeDeclarationAlgebraicDataType "Boolean" [])) [conditionExpression]
              conditionExpression <- getExpressionContainer conditionTypeHandler renderContext

              thenResult <- render renderContext thenStatements
              elseResult <- render renderContext elseStatements

              return
                JavaScriptDomResult
                  { create =
                      propertyToCode deleteCallback
                        ++ [Ln " = ("]
                        ++ propertyToCode deleteParameter
                        ++ [ Ln ") => {",
                             Ind
                               ( [ Ln "if ("
                                 ]
                                   ++ propertyToCode deleteParameter
                                   ++ [ Ln ") {",
                                        Ind (delete thenResult),
                                        Ln "} else {",
                                        Ind (delete elseResult),
                                        Ln "}",
                                        Br
                                      ]
                               ),
                             Ln "}",
                             Br
                           ]
                        ++ propertyToCode createCallback
                        ++ [Ln " = ("]
                        ++ propertyToCode createParameter
                        ++ [ Ln ") => {",
                             Ind
                               ( [ Ln "if ("
                                 ]
                                   ++ propertyToCode createParameter
                                   ++ [ Ln ") {",
                                        Ind (create thenResult),
                                        Ln "} else {",
                                        Ind (create elseResult),
                                        Ln "}",
                                        Br
                                      ]
                               ),
                             Ln "}",
                             Br
                           ]
                        ++ propertyToCode conditionCache
                        ++ [Ln " = "]
                        ++ getExpressionCode conditionExpression
                        ++ [ Ln ";",
                             Br
                           ]
                        ++ propertyToCode createCallback
                        ++ [Ln "("]
                        ++ propertyToCode conditionCache
                        ++ [Ln ");", Br],
                    update =
                      map
                        ( \dependency ->
                            ( dependency,
                              propertyToCode conditionCompareCache
                                ++ [Ln " = "]
                                ++ getExpressionCode conditionExpression
                                ++ [ Ln ";",
                                     Br,
                                     Ln "if ("
                                   ]
                                ++ propertyToCode (conditionCompareCache)
                                ++ [Ln " != "]
                                ++ propertyToCode conditionCache
                                ++ [ Ln ") {",
                                     Ind
                                       ( propertyToCode conditionCache
                                           ++ [Ln " = "]
                                           ++ propertyToCode conditionCompareCache
                                           ++ [ Ln ";",
                                                Br
                                              ]
                                           ++ propertyToCode deleteCallback
                                           ++ [Ln "(!"]
                                           ++ propertyToCode conditionCache
                                           ++ [ Ln ");",
                                                Br
                                              ]
                                           ++ propertyToCode createCallback
                                           ++ [Ln "("]
                                           ++ propertyToCode conditionCache
                                           ++ [ Ln ");"
                                              ]
                                       ),
                                     Ln "}",
                                     Br
                                   ]
                            )
                        )
                        (maybeToList (selfDependency conditionExpression) ++ extraDependencies conditionExpression)
                        ++ map (\(property, code) -> (property, Ln "if (" : propertyToCode conditionCache ++ [Ln ") {", Ind code, Ln "}"])) (update thenResult)
                        ++ map (\(property, code) -> (property, Ln "if (!" : propertyToCode conditionCache ++ [Ln ") {", Ind code, Ln "}"])) (update elseResult),
                    dealloc = [],
                    delete =
                      propertyToCode deleteCallback
                        ++ [Ln "("]
                        ++ propertyToCode conditionCache
                        ++ [ Ln ")"
                           ],
                    siblings = [SiblingCondition (propertyToCode conditionCache) (siblings thenResult) (siblings elseResult)]
                  },
            getExpressionContainer = \_ -> error "not yet implemented",
            call = \_ -> error "no functioncall available for Condition"
          }
   in Just result
javaScriptTypeHandlerConditionContainer typeHandlerContext _ _ = Nothing