module TypeChecker.Main where

import Data.Data (dataTypeName)
import Parser.Types (AST, ASTExpression, ASTExpression', ASTLeftHandSide (ASTLeftHandSideVariable), ASTStatement (ASTStatementVariableExpressionAssignment, ASTStatementVariableTypeAssignment), ASTTypeDeclaration (ASTTypeDeclarationAlgebraicDataType, ASTTypeDeclarationFunction, ASTTypeDeclarationRecord))
import TypeChecker.Types

typecheck :: TypeHandler a => [ASTExpression' -> Maybe a] -> [ASTStatement] -> Either String [GroupedStatement]
typecheck typeHandlerContainers ungroupedStatements =
  let groupedStatements = groupStatements ungroupedStatements
   in Right groupedStatements

groupStatements :: [ASTStatement] -> [GroupedStatement]
groupStatements [] = []
groupStatements ((ASTStatementVariableTypeAssignment variableName typeDeclaration) : restStatements) =
  case restStatements of
    ((ASTStatementVariableExpressionAssignment (ASTLeftHandSideVariable variableName') expression) : _) ->
      if variableName == variableName'
        then
          let ((TypedVariableAssignment Nothing nestGroupedStatements) : restGroupedStatements) = groupStatements restStatements
           in (TypedVariableAssignment (Just typeDeclaration) nestGroupedStatements : restGroupedStatements)
        else TypedVariableAssignment (Just typeDeclaration) [] : groupStatements restStatements
    _ -> TypedVariableAssignment (Just typeDeclaration) [] : groupStatements restStatements
groupStatements ((ASTStatementVariableExpressionAssignment leftHandSide rightHandSide) : restStatements) =
  case leftHandSide of
    ASTLeftHandSideVariable variableName ->
      case restStatements of
        ((ASTStatementVariableExpressionAssignment (ASTLeftHandSideVariable variableName') expression) : _) ->
          if variableName == variableName'
            then
              let ((TypedVariableAssignment Nothing nestGroupedStatements) : restGroupedStatements) = groupStatements restStatements
               in TypedVariableAssignment Nothing ((leftHandSide, rightHandSide) : nestGroupedStatements) : groupStatements restStatements
            else TypedVariableAssignment Nothing [(leftHandSide, rightHandSide)] : groupStatements restStatements
        _ -> TypedVariableAssignment Nothing [(leftHandSide, rightHandSide)] : groupStatements restStatements
    _ ->
      TypedVariableAssignment Nothing [(leftHandSide, rightHandSide)] : groupStatements restStatements
groupStatements (currentStatement : restStatements) = error ("this is not implemented yet " ++ show currentStatement)