module Prelude.Javascript.Types where

import Control.Monad.State.Lazy (State)
import Parser.Types (ASTExpression', ASTLeftHandSide, ASTTypeDeclaration)
import TypeChecker.Types (Property, TypeHandler (..), TypeHandlerContext, TypeValue)

data Code = Ln String | Ind [Code] | Br
  deriving (Show)

type VariableStackEntry = (String, [Property], JavaScriptTypeHandler)

type VariableStack = [VariableStackEntry]

data JavaScriptRenderContext = JavaScriptRenderContext
  { runParent :: String,
    runTypes :: [TypeHandlerContext JavaScriptTypeHandler -> Maybe ASTTypeDeclaration -> TypeValue -> Maybe JavaScriptTypeHandler],
    runStack :: VariableStack
  }

data JavaScriptDomResult = JavaScriptDomResult
  { create :: [Code],
    update :: [(String, [Code])],
    dealloc :: [Code],
    delete :: [Code]
  }

data JavaScriptTypeHandler = JavaScriptTypeHandler
  { destructure :: ASTLeftHandSide -> AppStateMonad [(VariableStackEntry, [Code])],
    getDom :: JavaScriptRenderContext -> AppStateMonad JavaScriptDomResult,
    getExpression :: JavaScriptRenderContext -> [Code] -> AppStateMonad JavaScriptExpressionResult
  }

instance TypeHandler JavaScriptTypeHandler

data JavaScriptExpressionResult = JavaScriptExpressionResult
  { expression :: [Code]
  }

type TypeHandlerContainer = TypeHandlerContext JavaScriptTypeHandler -> Maybe ASTTypeDeclaration -> TypeValue -> Maybe JavaScriptTypeHandler

data AppState = AppState
  { runExpressionId :: Int
  }

type AppStateMonad = State AppState