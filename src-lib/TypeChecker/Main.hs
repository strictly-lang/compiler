module TypeChecker.Main where

import Data.Data (dataTypeName)
import Parser.Types (ASTExpression)
import TypeChecker.Types

findTypehandler :: TypeHandler a => [ASTExpression -> Maybe a] -> ASTExpression -> Maybe a
findTypehandler [] expression = Nothing
findTypehandler (typeHandlerContainer : restTypeHandlersContainer) expression =
  case typeHandlerContainer expression of
    Just typeHandler -> Just typeHandler
    _ -> findTypehandler restTypeHandlersContainer expression
