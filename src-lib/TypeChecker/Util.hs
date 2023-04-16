module TypeChecker.Util where

import Parser.Types
import TypeChecker.Types

groupStatements :: [ASTStatement] -> [GroupedStatement]
groupStatements [] = []
groupStatements ((ASTStatementVariableTypeAssignment variableName typeDeclaration) : restStatements) =
  case restStatements of
    ((ASTStatementVariableExpressionAssignment (ASTLeftHandSideVariable variableName') expression) : _)
      | variableName == variableName' ->
          let ((GroupedStatementVariableAssignment Nothing nestGroupedStatements) : restGroupedStatements) = groupStatements restStatements
           in (GroupedStatementVariableAssignment (Just typeDeclaration) nestGroupedStatements : restGroupedStatements)
    _ -> GroupedStatementVariableAssignment (Just typeDeclaration) [] : groupStatements restStatements
groupStatements ((ASTStatementVariableExpressionAssignment leftHandSide rightHandSide) : restStatements) =
  case leftHandSide of
    ASTLeftHandSideVariable variableName ->
      case restStatements of
        ((ASTStatementVariableExpressionAssignment (ASTLeftHandSideVariable variableName') expression) : _)
          | variableName == variableName' ->
              let ((GroupedStatementVariableAssignment Nothing nestGroupedStatements) : restGroupedStatements) = groupStatements restStatements
               in GroupedStatementVariableAssignment Nothing ((leftHandSide, groupExpression rightHandSide) : nestGroupedStatements) : groupStatements restStatements
        _ -> GroupedStatementVariableAssignment Nothing [(leftHandSide, groupExpression rightHandSide)] : groupStatements restStatements
    _ ->
      GroupedStatementVariableAssignment Nothing [(leftHandSide, groupExpression rightHandSide)] : groupStatements restStatements
groupStatements ((ASTExpression expression) : restStatements) = GroupedExpression (groupExpression expression) : groupStatements restStatements
groupStatements (currentStatement : restStatements) = error ("this is not implemented yet " ++ show currentStatement)

groupExpression :: ASTExpression -> GroupedExpression
groupExpression = map groupExpression'

groupExpression' :: ASTExpression' -> GroupedExpression'
groupExpression' (ASTExpressionVariable variableName) = GroupedExpressionVariable variableName
groupExpression' (ASTExpressionList entities source) = GroupedExpressionList (map groupExpression entities) (groupStatements source)
groupExpression' (ASTExpressionRecord records) = GroupedExpressionRecord (groupRecord records)
groupExpression' (ASTExpressionAlgebraicDataType algebraicDataTypeName parameter) = GroupedExpressionAlgebraicDataType algebraicDataTypeName (map groupExpression parameter)
groupExpression' (ASTExpressionNumber number) = GroupedExpressionNumber number
groupExpression' (ASTExpressionRange from to) = GroupedExpressionRange from to
groupExpression' (ASTExpressionString stringParts) =
  GroupedExpressionString
    [ case stringPart of
        ASTStringStatic value -> GroupedStringStatic value
        ASTStringDynamic expression -> GroupedStringDynamic (groupExpression expression)
      | stringPart <- stringParts
    ]
groupExpression' (ASTExpressionFunctionDeclaration parameter body) = GroupedExpressionFunctionDeclaration parameter (groupStatements body)
groupExpression' (ASTExpressionFunctionCall parameter) = GroupedExpressionFunctionCall (map groupExpression parameter)
groupExpression' (ASTExpressionOperator operator leftExpression rightExpression) = GroupedExpressionOperator operator (groupExpression leftExpression) (groupExpression rightExpression)
groupExpression' (ASTExpressionCondition conditionExpression thenStatements elseStatements) = GroupedExpressionCondition (groupExpression conditionExpression) (groupStatements thenStatements) (groupStatements elseStatements)
groupExpression' (ASTExpressionMatch matchExpression cases) = GroupedExpressionMatch (groupExpression matchExpression) [(leftHandSideCase, groupStatements body) | (leftHandSideCase, body) <- cases]
groupExpression' (ASTExpressionHost hostname properties children) = GroupedExpressionHost hostname (groupRecord properties) (groupStatements children)
groupExpression' (ASTExpressionFragment children) = GroupedExpressionFragment (map groupExpression children)

groupRecord :: ASTRecord -> GroupedRecord
groupRecord (properties, source) = ([(propertyName, (typeDeclaration, [(condition, groupExpression propertyValue) | (condition, propertyValue) <- propertyValues])) | (propertyName, (typeDeclaration, propertyValues)) <- properties], groupStatements source)