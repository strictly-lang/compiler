module TypeChecker.Main where

import Data.Data (dataTypeName)
import Parser.Types (ASTExpression', ASTStatement)
import TypeChecker.Types
import TypeChecker.Util (groupStatements)

typecheck :: TypeHandler a => [ASTExpression' -> Maybe a] -> [ASTStatement] -> Either String [GroupedStatement]
typecheck typeHandlerContainers ungroupedStatements =
  let groupedStatements = groupStatements ungroupedStatements
   in Right groupedStatements
