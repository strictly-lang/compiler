module Prelude.Javascript.Types where

import TypeChecker.Types (TypeHandler (..))

data JavaScriptTypeHandler = JavaScriptTypeHandler
  { runName :: String
  }

instance TypeHandler JavaScriptTypeHandler where
  name = runName
