module TypeChecker.Types where

import Parser.Types (ASTExpression', ASTLeftHandSide, ASTTypeDeclaration)

data TypeValue a = TypeValueByLiteral ASTExpression' | TypeValueByReference a

data TypeHandlerContext a = TypeHandlerContext
  { runTypes :: [TypeHandlerContext a -> Maybe ASTTypeDeclaration -> TypeValue a -> Maybe a]
  }

class TypeHandler a where
  destructure :: a -> String
