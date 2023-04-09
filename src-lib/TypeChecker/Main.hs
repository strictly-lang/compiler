module TypeChecker.Main where

import Data.Data (dataTypeName)
import Parser.Types (AST, ASTExpression, ASTExpression', ASTLeftHandSide (ASTLeftHandSideVariable), ASTStatement (ASTStatementVariableExpressionAssignment, ASTStatementVariableTypeAssignment), ASTTypeDeclaration (ASTTypeDeclarationAlgebraicDataType, ASTTypeDeclarationFunction, ASTTypeDeclarationRecord))
import TypeChecker.Types

typecheck :: TypeHandler a => [ASTExpression' -> Maybe a] -> [ASTStatement] -> Either String [TypedStatement]
typecheck typeHandlerContainers [] =
  do return []
typecheck typeHandlerContainers (currentStatement@(ASTStatementVariableTypeAssignment variableName typeDeclaration) : restStatements) = do
  let (groupedAssignments, restStatements') = groupStatements currentStatement restStatements
  return []

groupStatements :: ASTStatement -> [ASTStatement] -> ((Maybe ASTTypeDeclaration, [ASTExpression]), [ASTStatement])
groupStatements (ASTStatementVariableTypeAssignment variableName typeDeclaration) restStatements =
  case restStatements of
    [] -> ((Just typeDeclaration, []), restStatements)
    (nestStatement@(ASTStatementVariableExpressionAssignment (ASTLeftHandSideVariable variableName') expression) : restStatements') ->
      if variableName == variableName'
        then
          let ((_, restGroupedStatements), restStatements'') = groupStatements nestStatement restStatements'
           in ((Just typeDeclaration, restGroupedStatements), restStatements'')
        else ((Just typeDeclaration, []), restStatements)
    _ -> ((Just typeDeclaration, []), restStatements)
