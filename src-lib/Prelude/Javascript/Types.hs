module Prelude.Javascript.Types where

import Parser.Types (ASTExpression' (ASTExpressionString))
import Prelude.Javascript.Util
import TypeChecker.Types (TypeHandler (..))

data JavaScriptTypeHandler = JavaScriptTypeHandler
  { getProperty :: String,
    getDom :: String -> [Code]
  }

instance TypeHandler JavaScriptTypeHandler where
  getProperty = Prelude.Javascript.Types.getProperty
