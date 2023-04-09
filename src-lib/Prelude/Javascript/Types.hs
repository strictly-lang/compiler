module Prelude.Javascript.Types where

import TypeChecker.Types (TypeHandler)

data JavascriptTypeHandler = JavascriptTypeHandler
  {
  }
  deriving (Show)

instance TypeHandler JavascriptTypeHandler
