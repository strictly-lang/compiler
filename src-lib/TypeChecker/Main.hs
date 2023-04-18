module TypeChecker.Main where

import Data.Data (dataTypeName)
import Data.Maybe (fromMaybe)
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
groupedStatementToTypedStatement typehandlerContainers stack (GroupedStatementVariableAssignment typeDefinition assignments) =
  let headTypeHandler = findTypeHandler typehandlerContainers (fromMaybe (getTypeDefinitionFromExpression stack (head (snd (head assignments)))) typeDefinition)
      typedAssignments =
        [ (leftHandSide, (headExpression, headTypeHandler) : getNestedTypeHandler stack headTypeHandler restNestedExpressions)
          | (leftHandSide, headExpression : restNestedExpressions) <- assignments,
            let headTypeHandler = findTypeHandler typehandlerContainers (getTypeDefinitionFromExpression stack headExpression)
        ]
      foo = last (snd (head typedAssignments))
   in ( TypedStatementVariableAssignment typedAssignments,
        getStackEntries (snd (last (snd (head typedAssignments)))) (map fst assignments) ++ stack
      )

findTypeHandler :: TypeHandler a => [TypeHandlerContainer a] -> ASTTypeDeclaration -> a
findTypeHandler [] typedefinition = error ("could not find typehandler for " ++ show typedefinition)
findTypeHandler (currentTypeHandlerContainer : restTypeHandlerContainers) typeDefinition =
  case currentTypeHandlerContainer typeDefinition of
    Just typeHandlerContainer -> typeHandlerContainer
    Nothing -> findTypeHandler restTypeHandlerContainers typeDefinition

getStackEntries :: TypeHandler a => a -> [ASTLeftHandSide] -> Stack a
getStackEntries typeHandler [] = []
getStackEntries typeHandler ((ASTLeftHandSideVariable name) : restLeftHandSides) = getStackEntries typeHandler restLeftHandSides ++ [(name, typeHandler)]

getTypeDefinitionFromExpression :: TypeHandler a => Stack a -> ASTExpression' -> ASTTypeDeclaration
getTypeDefinitionFromExpression stack (ASTExpressionVariable name) = error "no lookup implemented"
getTypeDefinitionFromExpression stack (ASTExpressionFunctionDeclaration parameters body) = ASTTypeDeclarationFunction [ASTTypeDeclarationGeneric (show index) | (parameter, index) <- zip parameters [0 ..]] (ASTTypeDeclarationGeneric (show (length parameters)))

getNestedTypeHandler :: TypeHandler a => Stack a -> a -> ASTExpression -> [(ASTExpression', a)]
getNestedTypeHandler stack typeHandler [] = []
getNestedTypeHandler stack typeHandler (currentExpression@(ASTExpressionVariable variableName) : restNestedExpressions) =
  let Just nestedTypeHandler = destructure typeHandler variableName
   in (currentExpression, nestedTypeHandler) : getNestedTypeHandler stack nestedTypeHandler restNestedExpressions
