module TypeChecker.Main where

import Data.Data (dataTypeName)
import Parser.Types (ASTExpression', ASTTypeDeclaration)
import TypeChecker.Types

findTypehandler :: TypeHandler a => TypeHandlerContext a b -> Maybe ASTTypeDeclaration -> [TypeValue b] -> Maybe a
findTypehandler typeHandlerContext = findTypehandler' typeHandlerContext (runTypes typeHandlerContext)

findTypehandler' :: TypeHandler a => TypeHandlerContext a b -> [TypeHandlerContext a b -> Maybe ASTTypeDeclaration -> [TypeValue b] -> Maybe a] -> Maybe ASTTypeDeclaration -> [TypeValue b] -> Maybe a
findTypehandler' typeHandlerContext [] typeDeclaration expression = Nothing
findTypehandler' typeHandlerContext (typeHandlerContainer : restTypeHandlersContainer) typeDeclaration expression =
  case typeHandlerContainer typeHandlerContext typeDeclaration expression of
    Just typeHandler -> Just typeHandler
    _ -> findTypehandler' typeHandlerContext restTypeHandlersContainer typeDeclaration expression
