module Prelude.Javascript.Types where

import Parser.Types (ASTTypeDeclaration)
import TypeChecker.Types
import WebcomponentEmitter.Types (AppStateMonad, Code)

data JavascriptTypeHandler = JavascriptTypeHandler
  { properties :: [TypeHandlerContainer JavascriptTypeHandler] -> [(String, JavascriptTypeHandler)],
    call :: [TypeHandlerContainer JavascriptTypeHandler] -> Stack JavascriptTypeHandler -> [JavascriptTypeHandler] -> (JavascriptTypeHandler, ASTTypeDeclaration -> AppStateMonad FunctionBuildResult),
    getTypeDeclaration :: ASTTypeDeclaration,
    getDom :: [Code],
    getCode :: [Code]
  }

data FunctionBuildResult = FunctionBuildResult
  { create :: [Code],
    reconcile :: [Code]
  }

instance TypeHandler JavascriptTypeHandler where
  properties = Prelude.Javascript.Types.properties
  getTypeDeclaration = Prelude.Javascript.Types.getTypeDeclaration
  call typeHandlerContainer typeHandlerContainers stack parameters = fst (Prelude.Javascript.Types.call typeHandlerContainer typeHandlerContainers stack parameters)

instance Show JavascriptTypeHandler where
  show a = "JavascriptTypeHandler"
