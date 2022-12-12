module TypeChecker.Types where

import Parser.Types (ASTExpression', ASTLeftHandSide, ASTTypeDeclaration)

data TypeValue a = TypeValueByLiteral ASTExpression' | TypeValueByReference a

data TypeHandlerContext a b = TypeHandlerContext
  { runTypes :: [TypeHandlerContext a b -> Maybe ASTTypeDeclaration -> TypeValue b -> Maybe a]
  }

class TypeHandler a where
  destructure :: a -> String
