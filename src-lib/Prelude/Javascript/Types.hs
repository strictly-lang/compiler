module Prelude.Javascript.Types where

import Control.Monad.State.Lazy (State)
import Parser.Types (ASTExpression, ASTExpression', ASTLeftHandSide, ASTTypeDeclaration)
import TypeChecker.Types (TypeHandler (..), TypeHandlerContext, TypeValue)

data Property = DotNotation String | BracketNotation String
  deriving (Eq, Show)

data Code = Ln String | Ind [Code] | Br
  deriving (Show)

data Sibling = SiblingAlways [Property] | SiblingCondition [Code] [Sibling] [Sibling]

type VariableStackEntry = (String, Maybe [Property], JavaScriptTypeHandler)

type VariableStack = [VariableStackEntry]

data JavaScriptRenderContext = JavaScriptRenderContext
  { runParent :: [Property],
    runSiblings :: [Sibling],
    runTypes :: [TypeHandlerContext JavaScriptTypeHandler JavaScriptExpressionResult -> Maybe ASTTypeDeclaration -> [TypeValue JavaScriptExpressionResult] -> Maybe JavaScriptTypeHandler],
    runStack :: VariableStack,
    runScope :: [Property]
  }

data JavaScriptDomResult = JavaScriptDomResult
  { create :: [Code],
    update :: [([Property], [Code])],
    dealloc :: [Code],
    delete :: [Code],
    siblings :: [Sibling]
  }

data JavaScriptTypeHandler = JavaScriptTypeHandler
  { destructure :: JavaScriptRenderContext -> ASTLeftHandSide -> AppStateMonad [(VariableStackEntry, [Code])],
    getDom :: JavaScriptRenderContext -> AppStateMonad JavaScriptDomResult,
    getExpressionContainer :: JavaScriptRenderContext -> AppStateMonad JavaScriptExpressionResult,
    call :: JavaScriptRenderContext -> [ASTExpression] -> AppStateMonad JavaScriptTypeHandler
  }

instance TypeHandler JavaScriptTypeHandler

data JavaScriptExpressionResult = JavaScriptExpressionResult
  { getExpressionCode :: [Code],
    selfDependency :: Maybe [Property],
    extraDependencies :: [[Property]]
  }

type TypeHandlerContainer = TypeHandlerContext JavaScriptTypeHandler JavaScriptExpressionResult -> Maybe ASTTypeDeclaration -> [TypeValue JavaScriptExpressionResult] -> Maybe JavaScriptTypeHandler

data AppState = AppState
  { runExpressionId :: Int
  }

type AppStateMonad = State AppState