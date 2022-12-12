module Prelude.Javascript.Types where

import Control.Monad.State.Lazy (State)
import Parser.Types (ASTExpression', ASTLeftHandSide, ASTTypeDeclaration)
import TypeChecker.Types (TypeHandler (..), TypeHandlerContext, TypeValue)

data Code = Ln String | Ind [Code] | Br
  deriving (Show)

type VariableStackEntry = (String, [Code], JavaScriptTypeHandler)

type VariableStack = [VariableStackEntry]

data JavaScriptRenderContext = JavaScriptRenderContext
  { runParent :: String,
    runTypes :: [TypeHandlerContext JavaScriptTypeHandler -> Maybe ASTTypeDeclaration -> TypeValue JavaScriptTypeHandler -> Maybe JavaScriptTypeHandler],
    runStack :: VariableStack
  }

data JavaScriptDomResult = JavaScriptDomResult
  { create :: [Code],
    update :: [(String, [Code])],
    dealloc :: [Code],
    delete :: [Code]
  }

data JavaScriptTypeHandler = JavaScriptTypeHandler
  { destructure :: JavaScriptRenderContext -> ASTLeftHandSide -> AppStateMonad [(VariableStackEntry, [Code])],
    getDom :: JavaScriptRenderContext -> AppStateMonad JavaScriptDomResult,
    getExpressionContainer :: JavaScriptRenderContext -> AppStateMonad JavaScriptExpressionResult
  }

instance TypeHandler JavaScriptTypeHandler

data JavaScriptExpressionResult = JavaScriptExpressionResult
  { getExpressionCode :: [Code]
  }

type TypeHandlerContainer = TypeHandlerContext JavaScriptTypeHandler -> Maybe ASTTypeDeclaration -> TypeValue JavaScriptTypeHandler -> Maybe JavaScriptTypeHandler

data AppState = AppState
  { runExpressionId :: Int
  }

type AppStateMonad = State AppState