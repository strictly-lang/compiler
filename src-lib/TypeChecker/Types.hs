module TypeChecker.Types where

import Parser.Types (ASTExpression, ASTExpression', ASTLeftHandSide, ASTTypeDeclaration)

data TypeHandlerContext a = TypeHandlerContext
  { runTypes :: [TypeHandlerContext a -> Maybe ASTTypeDeclaration -> ASTExpression -> Maybe a]
  }

class TypeHandler a where
  getProperty :: a -> String
