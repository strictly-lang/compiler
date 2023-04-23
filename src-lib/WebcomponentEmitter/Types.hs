module WebcomponentEmitter.Types where

import Control.Monad.State.Lazy (State)
import Parser.Types (ASTExpression', ASTTypeDeclaration)

data Property = DotNotation String | BracketNotation String
  deriving (Eq, Show)

data Code = Ln String | Ind [Code] | Inl [Code] | Br
  deriving (Show)

data Sibling = SiblingAlways [Property] | SiblingCondition [Code] [Sibling] [Sibling]

data AppState = AppState
  { runExpressionId :: Int
  }

type AppStateMonad = State AppState