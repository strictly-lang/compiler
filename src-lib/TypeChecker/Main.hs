module TypeChecker.Main where

import Data.Data (dataTypeName)
import Parser.Types
import TypeChecker.Types

typecheck :: TypeHandler a => [TypeHandlerContainer a] -> Stack a -> [ASTStatement] -> Either String [TypedStatement a]
typecheck typeHandlerContainers stack ungroupedStatements =
  let groupedStatements = walkStatements typeHandlerContainers stack ungroupedStatements
   in Right groupedStatements

walkStatements :: TypeHandler a => [TypeHandlerContainer a] -> Stack a -> [ASTStatement] -> [TypedStatement a]
walkStatements typeHandlerContainers stack statements =
  case groupStatements statements of
    (Nothing, []) ->
      []
    (Just groupedStatement, restStatements) ->
      let (typedStatement, stack') = groupedStatementToTypedStatement typeHandlerContainers stack groupedStatement
       in typedStatement : walkStatements typeHandlerContainers stack' restStatements

groupStatements :: [ASTStatement] -> (Maybe GroupedStatement, [ASTStatement])
groupStatements [] = (Nothing, [])
groupStatements ((ASTStatementVariableTypeAssignment variableName typeDeclaration) : restStatements) =
  case restStatements of
    ((ASTStatementVariableExpressionAssignment (ASTLeftHandSideVariable variableName') expression) : _)
      | variableName == variableName' ->
          let (Just (GroupedStatementVariableAssignment Nothing nestGroupedStatements), restGroupedStatements) = groupStatements restStatements
           in (Just (GroupedStatementVariableAssignment (Just typeDeclaration) nestGroupedStatements), restGroupedStatements)
    _ -> (Just (GroupedStatementVariableAssignment (Just typeDeclaration) []), restStatements)
groupStatements ((ASTStatementVariableExpressionAssignment leftHandSide rightHandSide) : restStatements) =
  case (leftHandSide, restStatements) of
    (ASTLeftHandSideVariable variableName, (ASTStatementVariableExpressionAssignment (ASTLeftHandSideVariable variableName') expression) : _)
      | variableName == variableName' ->
          let (Just (GroupedStatementVariableAssignment Nothing nestGroupedStatements), restGroupedStatements) = groupStatements restStatements
           in (Just (GroupedStatementVariableAssignment Nothing ((leftHandSide, rightHandSide) : nestGroupedStatements)), restStatements)
    _ -> (Just (GroupedStatementVariableAssignment Nothing [(leftHandSide, rightHandSide)]), restStatements)
groupStatements ((ASTExpression expression) : restStatements) = (Just (GroupedExpression expression), restStatements)
groupStatements (currentStatement : restStatements) = error ("this is not implemented yet " ++ show currentStatement)

groupedStatementToTypedStatement :: TypeHandler a => [TypeHandlerContainer a] -> Stack a -> GroupedStatement -> (TypedStatement a, Stack a)
groupedStatementToTypedStatement [] stack groupedStatement = error ("could not find typehandler container for " ++ show groupedStatement)
groupedStatementToTypedStatement (currentTypeHandlerContainer : restTypeHandlerContainers) stack groupedStatement@(GroupedStatementVariableAssignment typeDefinition assignments) =
  case currentTypeHandlerContainer typeDefinition (map snd assignments) of
    Just typeHandlerContainer ->
      (TypedStatementVariableAssignment typeHandlerContainer, getStackEntries typeHandlerContainer (map fst assignments) ++ stack)
    Nothing ->
      groupedStatementToTypedStatement restTypeHandlerContainers stack groupedStatement

getStackEntries :: TypeHandler a => a -> [ASTLeftHandSide] -> Stack a
getStackEntries typeHandler [] = []
getStackEntries typeHandler ((ASTLeftHandSideVariable name) : restLeftHandSides) = getStackEntries typeHandler restLeftHandSides ++ [(name, typeHandler)]