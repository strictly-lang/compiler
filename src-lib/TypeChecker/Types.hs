module TypeChecker.Types where

import Parser.Types (ASTExpression, ASTExpression', ASTLeftHandSide)

data TypeHandlerContext a = TypeHandlerContext
  { runTypes :: [TypeHandlerContext a -> ASTExpression -> Maybe a]
  }

class TypeHandler a where
  getProperty :: a -> String
