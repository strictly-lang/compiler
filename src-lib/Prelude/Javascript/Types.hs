module Prelude.Javascript.Types where

import Control.Monad.State.Lazy (State)
import Parser.Types (ASTExpression, ASTExpression' (ASTExpressionString))
import TypeChecker.Types (TypeHandler (..), TypeHandlerContext)

data Code = Ln String | Ind [Code] | Br
  deriving (Show)

data JavaScriptTypeHandler = JavaScriptTypeHandler
  { getProperty :: String,
    getDom :: JavaScriptRenderContext -> AppStateMonad JavaScriptDomResult
  }

data JavaScriptDomResult = JavaScriptDomResult
  { create :: [Code],
    update :: [(String, [Code])],
    dealloc :: [Code],
    delete :: [Code]
  }

data JavaScriptRenderContext = JavaScriptRenderContext
  { runParent :: String,
    runTypes :: [TypeHandlerContext JavaScriptTypeHandler -> ASTExpression -> Maybe JavaScriptTypeHandler]
  }

instance TypeHandler JavaScriptTypeHandler where
  getProperty = Prelude.Javascript.Types.getProperty

type TypeHandlerContainer = TypeHandlerContext JavaScriptTypeHandler -> ASTExpression -> Maybe JavaScriptTypeHandler

data AppState = AppState
  { runExpressionId :: Int
  }

type AppStateMonad = State AppState