module Prelude.Javascript.Types.Function where

import Data.List (intercalate)
import Parser.Types (ASTExpression, ASTExpression' (ASTExpressionFunctionDeclaration), ASTStatement (ASTExpression, ASTStatementVariableAssignment), ASTTypeDeclaration (ASTTypeDeclarationFunction))
import Prelude.Javascript.Types
import Prelude.Javascript.Util (getGetFreshExprId, nestedExpression, propertyToCode)
import TypeChecker.Main (findTypehandler)
import TypeChecker.Types (TypeValue (TypeValueByLiteral, TypeValueByReference))

functionBody :: JavaScriptRenderContext -> [ASTStatement] -> AppStateMonad JavaScriptExpressionResult
functionBody renderContext [] = do
  return
    JavaScriptExpressionResult
      { getExpressionCode = [],
        selfDependency = Nothing,
        extraDependencies = []
      }
functionBody renderContext ((ASTExpression expression) : restStatements) = do
  resultContainer <- nestedExpression renderContext Nothing [expression]
  result <- getExpressionContainer resultContainer renderContext
  nextResult <- functionBody renderContext restStatements

  return
    JavaScriptExpressionResult
      { getExpressionCode = getExpressionCode result ++ getExpressionCode nextResult,
        selfDependency = Nothing, -- TODO
        extraDependencies = extraDependencies result ++ extraDependencies nextResult
      }
functionBody renderContext (ASTStatementVariableAssignment leftHandSide expression : restStatements) = do
  error "not yet implemented"

javaScriptTypeHandlerFunctionContainer :: TypeHandlerContainer
javaScriptTypeHandlerFunctionContainer typeHandlerContext (Just (ASTTypeDeclarationFunction parameterTypes bodyType)) (TypeValueByLiteral (ASTExpressionFunctionDeclaration parameters body) : restTypeValues) =
  Just
    ( JavaScriptTypeHandler
        { destructure = error "no property access implemented",
          getDom = \renderContext -> error "a function cant be used in dom",
          getExpressionContainer = \renderContext -> do
            parameterHandler <-
              mapM
                ( \(parameterType, parameter) -> do
                    exprId <- getGetFreshExprId
                    let parameterScope = [DotNotation ("parameter" ++ show exprId)]

                    let Just typeHandler =
                          findTypehandler
                            typeHandlerContext
                            (Just parameterType)
                            [ TypeValueByReference
                                ( JavaScriptExpressionResult
                                    { getExpressionCode = propertyToCode parameterScope,
                                      selfDependency = Just parameterScope,
                                      extraDependencies = []
                                    }
                                )
                            ]

                    variableStack' <- destructure typeHandler renderContext parameter
                    return (parameterScope, variableStack')
                )
                (zip parameterTypes parameters)
            let variableStack' = reverse (map fst (concatMap snd parameterHandler)) ++ runStack renderContext
            bodyResult <-
              functionBody
                ( JavaScriptRenderContext
                    { runParent =
                        runParent renderContext,
                      runTypes = runTypes renderContext,
                      runStack = runStack renderContext,
                      runScope = runScope renderContext
                    }
                )
                body

            return
              JavaScriptExpressionResult
                { getExpressionCode =
                    Ln "(("
                      : intercalate [Ln ", "] (map (propertyToCode . fst) parameterHandler)
                      ++ [ Ln ") => {",
                           Ind
                             (getExpressionCode bodyResult),
                           Ln "})"
                         ],
                  selfDependency = Nothing,
                  extraDependencies = [] -- TODO
                },
          call = \_ -> error "not yet implemented"
        }
    )
javaScriptTypeHandlerFunctionContainer typeHandlerContext (Just (ASTTypeDeclarationFunction parameterTypes bodyType)) ((TypeValueByReference referenceExpressionResult) : restTypeValues) =
  Just
    JavaScriptTypeHandler
      { destructure = error "no property access implemented",
        getDom = \renderContext -> error "a function cant be used in dom",
        getExpressionContainer = \_ -> do return referenceExpressionResult,
        call = \renderContext parameterExpressions -> do
          let functionExpressionResult = getExpressionCode referenceExpressionResult
          parameters <-
            mapM
              ( \(parameterType, parameterExpression) -> do
                  result <- nestedExpression renderContext (Just parameterType) ([parameterExpression])
                  return result
              )
              (zip parameterTypes parameterExpressions)
          let Just resultTypeHandler =
                findTypehandler
                  typeHandlerContext
                  (Just bodyType)
                  [ TypeValueByReference
                      ( JavaScriptExpressionResult
                          { getExpressionCode =
                              getExpressionCode referenceExpressionResult ++ [Ln "()"],
                            selfDependency = Nothing,
                            extraDependencies = [] -- TODO
                          }
                      )
                  ]
          return resultTypeHandler
      }
javaScriptTypeHandlerFunctionContainer typeHandlerContext _ _ = Nothing