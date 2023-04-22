module Prelude.Javascript.Types where

import Parser.Types (ASTTypeDeclaration)
import TypeChecker.Types

data JavascriptTypeHandler = JavascriptTypeHandler
  { properties :: [TypeHandlerContainer JavascriptTypeHandler] -> [(String, JavascriptTypeHandler)],
    call :: [TypeHandlerContainer JavascriptTypeHandler] -> Stack JavascriptTypeHandler -> [JavascriptTypeHandler] -> JavascriptTypeHandler,
    getTypeDeclaration :: ASTTypeDeclaration,
    getDom :: String,
    getCode :: String
  }

instance TypeHandler JavascriptTypeHandler where
  properties = Prelude.Javascript.Types.properties
  call = Prelude.Javascript.Types.call
  getTypeDeclaration = Prelude.Javascript.Types.getTypeDeclaration

instance Show JavascriptTypeHandler where
  show a = "JavascriptTypeHandler"
