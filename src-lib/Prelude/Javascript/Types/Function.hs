module Prelude.Javascript.Types.Function where

import Data.List (intercalate)
import Parser.Types (ASTExpression' (ASTExpressionFunctionDeclaration), ASTTypeDeclaration (ASTTypeDeclarationFunction))
import Prelude.Javascript.Types
import Prelude.Javascript.Util (getGetFreshExprId, propertyToCode)
import TypeChecker.Main (findTypehandler)
import TypeChecker.Types (TypeValue (TypeValueByLiteral, TypeValueByReference))

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

            return
              JavaScriptExpressionResult
                { getExpressionCode =
                    Ln "(("
                      : intercalate [Ln ", "] (map (propertyToCode . fst) parameterHandler)
                      ++ [ Ln ") => {",
                           Ind
                             [ let foo = ""
                                in Ln foo
                             ],
                           Ln "})"
                         ],
                  selfDependency = Nothing,
                  extraDependencies = [] -- TODO
                }
        }
    )
javaScriptTypeHandlerFunctionContainer typeHandlerContext _ _ = Nothing