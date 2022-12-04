module TypeChecker.Main where

import Data.Data (dataTypeName)
import Parser.Types (ASTExpression)
import TypeChecker.Types

findTypehandler :: TypeHandler a => TypeHandlerContext a -> ASTExpression -> Maybe a
findTypehandler typeHandlerContext = findTypehandler' typeHandlerContext (runTypes typeHandlerContext)

findTypehandler' :: TypeHandler a => TypeHandlerContext a -> [TypeHandlerContext a -> ASTExpression -> Maybe a] -> ASTExpression -> Maybe a
findTypehandler' typeHandlerContext [] expression = Nothing
findTypehandler' typeHandlerContext (typeHandlerContainer : restTypeHandlersContainer) expression =
  case typeHandlerContainer typeHandlerContext expression of
    Just typeHandler -> Just typeHandler
    _ -> findTypehandler' typeHandlerContext restTypeHandlersContainer expression
