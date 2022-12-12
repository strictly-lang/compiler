module TypeChecker.Types where

import Parser.Types (ASTExpression', ASTLeftHandSide, ASTTypeDeclaration)

data Property = DotNotation String | BracketNotation String
  deriving (Eq)

data TypeValue = TypeValueByLiteral ASTExpression' | TypeValueByReference [Property]

data TypeHandlerContext a = TypeHandlerContext
  { runTypes :: [TypeHandlerContext a -> Maybe ASTTypeDeclaration -> TypeValue -> Maybe a]
  }

class TypeHandler a where
  destructure :: a -> String
