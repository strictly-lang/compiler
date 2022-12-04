module Prelude.Javascript.Types where

import Control.Monad.State.Lazy (State)
import Parser.Types (ASTExpression, ASTExpression' (ASTExpressionString))
import TypeChecker.Types (TypeHandler (..), TypeHandlerContext)

data Code = Ln String | Ind [Code] | Br
  deriving (Show)

data JavaScriptTypeHandler = JavaScriptTypeHandler
  { getProperty :: String,
    getDom :: JavaScriptRenderContext -> AppStateMonad [Code]
  }

data JavaScriptRenderContext = JavaScriptRenderContext
  { runParent :: String,
    runTypes :: [TypeHandlerContext JavaScriptTypeHandler -> ASTExpression -> Maybe JavaScriptTypeHandler]
  }

instance TypeHandler JavaScriptTypeHandler where
  getProperty = Prelude.Javascript.Types.getProperty

data AppState = AppState
  { runExpressionId :: Int
  }

type AppStateMonad = State AppState