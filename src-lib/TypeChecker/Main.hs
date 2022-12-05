module TypeChecker.Main where

import Data.Data (dataTypeName)
import Parser.Types (ASTExpression', ASTTypeDeclaration)
import TypeChecker.Types

findTypehandler :: TypeHandler a => TypeHandlerContext a -> Maybe ASTTypeDeclaration -> ASTExpression' -> Maybe a
findTypehandler typeHandlerContext typeDeclaration = findTypehandler' typeHandlerContext (runTypes typeHandlerContext) typeDeclaration

findTypehandler' :: TypeHandler a => TypeHandlerContext a -> [TypeHandlerContext a -> Maybe ASTTypeDeclaration -> ASTExpression' -> Maybe a] -> Maybe ASTTypeDeclaration -> ASTExpression' -> Maybe a
findTypehandler' typeHandlerContext [] typeDeclaration expression = Nothing
findTypehandler' typeHandlerContext (typeHandlerContainer : restTypeHandlersContainer) typeDeclaration expression =
  case typeHandlerContainer typeHandlerContext typeDeclaration expression of
    Just typeHandler -> Just typeHandler
    _ -> findTypehandler' typeHandlerContext restTypeHandlersContainer typeDeclaration expression
