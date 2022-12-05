module TypeChecker.Types where

import Parser.Types (ASTExpression', ASTLeftHandSide, ASTTypeDeclaration)

data TypeHandlerContext a = TypeHandlerContext
  { runTypes :: [TypeHandlerContext a -> Maybe ASTTypeDeclaration -> ASTExpression' -> Maybe a]
  }

class TypeHandler a where
  destructure :: a -> String
