module Prelude.Javascript.Types where

import Control.Monad.State.Lazy (State)
import Parser.Types (ASTExpression', ASTLeftHandSide, ASTTypeDeclaration)
import TypeChecker.Types (TypeHandler (..), TypeHandlerContext)

data Code = Ln String | Ind [Code] | Br
  deriving (Show)

data Property = DotNotation String | BracketNotation String
  deriving (Eq)

type VariableStackEntry = (String, [Property], JavaScriptTypeHandler)

type VariableStack = [VariableStackEntry]

data JavaScriptTypeHandler = JavaScriptTypeHandler
  { destructure :: [Property] -> ASTLeftHandSide -> [(VariableStack, [Code])],
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
    runTypes :: [TypeHandlerContext JavaScriptTypeHandler -> Maybe ASTTypeDeclaration -> ASTExpression' -> Maybe JavaScriptTypeHandler]
  }

instance TypeHandler JavaScriptTypeHandler

type TypeHandlerContainer = TypeHandlerContext JavaScriptTypeHandler -> Maybe ASTTypeDeclaration -> ASTExpression' -> Maybe JavaScriptTypeHandler

data AppState = AppState
  { runExpressionId :: Int
  }

type AppStateMonad = State AppState