module TypeChecker.Main where

import Data.Data (dataTypeName)
import Parser.Types
import TypeChecker.Types

typecheck :: TypeHandler a => [TypeHandlerContainer a] -> Stack a -> [ASTStatement] -> Either String [GroupedStatement]
typecheck typeHandlerContainers stack ungroupedStatements =
  let groupedStatements = walkStatements typeHandlerContainers stack ungroupedStatements
   in Right groupedStatements

walkStatements :: TypeHandler a => [TypeHandlerContainer a] -> Stack a -> [ASTStatement] -> [GroupedStatement]
walkStatements typeHandlerContainers stack [] = []
walkStatements typeHandlerContainers stack ((ASTStatementVariableTypeAssignment variableName typeDeclaration) : restStatements) =
  case restStatements of
    ((ASTStatementVariableExpressionAssignment (ASTLeftHandSideVariable variableName') expression) : _)
      | variableName == variableName' ->
          let ((GroupedStatementVariableAssignment Nothing nestGroupedStatements) : restGroupedStatements) = walkStatements typeHandlerContainers stack restStatements
           in (GroupedStatementVariableAssignment (Just typeDeclaration) nestGroupedStatements : restGroupedStatements)
    _ -> GroupedStatementVariableAssignment (Just typeDeclaration) [] : walkStatements typeHandlerContainers stack restStatements
walkStatements typeHandlerContainers stack ((ASTStatementVariableExpressionAssignment leftHandSide rightHandSide) : restStatements) =
  case leftHandSide of
    ASTLeftHandSideVariable variableName ->
      case restStatements of
        ((ASTStatementVariableExpressionAssignment (ASTLeftHandSideVariable variableName') expression) : _)
          | variableName == variableName' ->
              let ((GroupedStatementVariableAssignment Nothing nestGroupedStatements) : restGroupedStatements) = walkStatements typeHandlerContainers stack restStatements
               in GroupedStatementVariableAssignment Nothing ((leftHandSide, walkExpression typeHandlerContainers stack rightHandSide) : nestGroupedStatements) : walkStatements typeHandlerContainers stack restStatements
        _ -> GroupedStatementVariableAssignment Nothing [(leftHandSide, walkExpression typeHandlerContainers stack rightHandSide)] : walkStatements typeHandlerContainers stack restStatements
    _ ->
      GroupedStatementVariableAssignment Nothing [(leftHandSide, walkExpression typeHandlerContainers stack rightHandSide)] : walkStatements typeHandlerContainers stack restStatements
walkStatements typeHandlerContainers stack ((ASTExpression expression) : restStatements) = GroupedExpression (walkExpression typeHandlerContainers stack expression) : walkStatements typeHandlerContainers stack restStatements
walkStatements typeHandlerContainers stack (currentStatement : restStatements) = error ("this is not implemented yet " ++ show currentStatement)

walkExpression :: TypeHandler a => [TypeHandlerContainer a] -> Stack a -> ASTExpression -> GroupedExpression
walkExpression typeHandlerContainers stack = map (walkExpression' typeHandlerContainers stack)

walkExpression' :: TypeHandler a => [TypeHandlerContainer a] -> Stack a -> ASTExpression' -> GroupedExpression'
walkExpression' typeHandlerContainers stack (ASTExpressionVariable variableName) = GroupedExpressionVariable variableName
walkExpression' typeHandlerContainers stack (ASTExpressionList entities source) = GroupedExpressionList (map (walkExpression typeHandlerContainers stack) entities) (walkStatements typeHandlerContainers stack source)
walkExpression' typeHandlerContainers stack (ASTExpressionRecord records) = GroupedExpressionRecord (groupRecord typeHandlerContainers stack records)
walkExpression' typeHandlerContainers stack (ASTExpressionAlgebraicDataType algebraicDataTypeName parameter) = GroupedExpressionAlgebraicDataType algebraicDataTypeName (map (walkExpression typeHandlerContainers stack) parameter)
walkExpression' typeHandlerContainers stack (ASTExpressionNumber number) = GroupedExpressionNumber number
walkExpression' typeHandlerContainers stack (ASTExpressionRange from to) = GroupedExpressionRange from to
walkExpression' typeHandlerContainers stack (ASTExpressionString stringParts) =
  GroupedExpressionString
    [ case stringPart of
        ASTStringStatic value -> GroupedStringStatic value
        ASTStringDynamic expression -> GroupedStringDynamic (walkExpression typeHandlerContainers stack expression)
      | stringPart <- stringParts
    ]
walkExpression' typeHandlerContainers stack (ASTExpressionFunctionDeclaration parameter body) = GroupedExpressionFunctionDeclaration parameter (walkStatements typeHandlerContainers stack body)
walkExpression' typeHandlerContainers stack (ASTExpressionFunctionCall parameter) = GroupedExpressionFunctionCall (map (walkExpression typeHandlerContainers stack) parameter)
walkExpression' typeHandlerContainers stack (ASTExpressionOperator operator leftExpression rightExpression) = GroupedExpressionOperator operator (walkExpression typeHandlerContainers stack leftExpression) (walkExpression typeHandlerContainers stack rightExpression)
walkExpression' typeHandlerContainers stack (ASTExpressionCondition conditionExpression thenStatements elseStatements) = GroupedExpressionCondition (walkExpression typeHandlerContainers stack conditionExpression) (walkStatements typeHandlerContainers stack thenStatements) (walkStatements typeHandlerContainers stack elseStatements)
walkExpression' typeHandlerContainers stack (ASTExpressionMatch matchExpression cases) = GroupedExpressionMatch (walkExpression typeHandlerContainers stack matchExpression) [(leftHandSideCase, walkStatements typeHandlerContainers stack body) | (leftHandSideCase, body) <- cases]
walkExpression' typeHandlerContainers stack (ASTExpressionHost hostname properties children) = GroupedExpressionHost hostname (groupRecord typeHandlerContainers stack properties) (walkStatements typeHandlerContainers stack children)
walkExpression' typeHandlerContainers stack (ASTExpressionFragment children) = GroupedExpressionFragment (map (walkExpression typeHandlerContainers stack) children)

groupRecord :: TypeHandler a => [TypeHandlerContainer a] -> Stack a -> ASTRecord -> GroupedRecord
groupRecord typeHandlerContainers stack (properties, source) = ([(propertyName, (typeDeclaration, [(condition, walkExpression typeHandlerContainers stack propertyValue) | (condition, propertyValue) <- propertyValues])) | (propertyName, (typeDeclaration, propertyValues)) <- properties], walkStatements typeHandlerContainers stack source)