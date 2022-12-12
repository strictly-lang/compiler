module TypeChecker.Main where

import Data.Data (dataTypeName)
import Parser.Types (ASTExpression', ASTTypeDeclaration)
import TypeChecker.Types

findTypehandler :: TypeHandler a => TypeHandlerContext a -> Maybe ASTTypeDeclaration -> TypeValue a -> Maybe a
findTypehandler typeHandlerContext = findTypehandler' typeHandlerContext (runTypes typeHandlerContext)

findTypehandler' :: TypeHandler a => TypeHandlerContext a -> [TypeHandlerContext a -> Maybe ASTTypeDeclaration -> TypeValue a -> Maybe a] -> Maybe ASTTypeDeclaration -> TypeValue a -> Maybe a
findTypehandler' typeHandlerContext [] typeDeclaration expression = Nothing
findTypehandler' typeHandlerContext (typeHandlerContainer : restTypeHandlersContainer) typeDeclaration expression =
  case typeHandlerContainer typeHandlerContext typeDeclaration expression of
    Just typeHandler -> Just typeHandler
    _ -> findTypehandler' typeHandlerContext restTypeHandlersContainer typeDeclaration expression
