module Prelude.Javascript.Types where

import Control.Monad.State.Lazy (State)
import Parser.Types (ASTExpression', ASTLeftHandSide, ASTTypeDeclaration)
import TypeChecker.Types (TypeHandler (..), TypeHandlerContext, TypeValue)

data Property = DotNotation String | BracketNotation String
  deriving (Eq, Show)

data Code = Ln String | Ind [Code] | Br
  deriving (Show)

type VariableStackEntry = (String, Maybe [Property], JavaScriptTypeHandler)

type VariableStack = [VariableStackEntry]

data JavaScriptRenderContext = JavaScriptRenderContext
  { runParent :: [Property],
    runTypes :: [TypeHandlerContext JavaScriptTypeHandler JavaScriptExpressionResult -> Maybe ASTTypeDeclaration -> TypeValue JavaScriptExpressionResult -> Maybe JavaScriptTypeHandler],
    runStack :: VariableStack,
    runScope :: [Property]
  }

data JavaScriptDomResult = JavaScriptDomResult
  { create :: [Code],
    update :: [([Property], [Code])],
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
  { getExpressionCode :: [Code],
    selfDependency :: Maybe [Property],
    extraDependencies :: [[Property]]
  }

type TypeHandlerContainer = TypeHandlerContext JavaScriptTypeHandler JavaScriptExpressionResult -> Maybe ASTTypeDeclaration -> TypeValue JavaScriptExpressionResult -> Maybe JavaScriptTypeHandler

data AppState = AppState
  { runExpressionId :: Int
  }

type AppStateMonad = State AppState