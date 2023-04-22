module TypeChecker.Main where

import Data.Data (dataTypeName)
import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Parser.Types
import TypeChecker.Types
import WebcomponentEmitter.Types (Code)

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
  let headTypeHandler = findTypeHandler typehandlerContainers (fromMaybe (getTypeDefinitionFromExpression stack (head (snd (head assignments)))) typeDefinition) (Right [headExpression | (_, headExpression : restExpressions) <- assignments])
      typedAssignments =
        [ (leftHandSide, (headExpression, headTypeHandler) : getNestedTypeHandler typehandlerContainers stack headTypeHandler restNestedExpressions)
          | (leftHandSide, headExpression : restNestedExpressions) <- assignments,
            let headTypeHandler = findTypeHandler typehandlerContainers (getTypeDefinitionFromExpression stack headExpression) (Right [headExpression])
        ]
   in ( TypedStatementVariableAssignment typedAssignments,
        getStackEntries (snd (last (snd (head typedAssignments)))) (head (map fst assignments)) ++ stack
      )

findTypeHandler :: TypeHandler a => [TypeHandlerContainer a] -> ASTTypeDeclaration -> Either [Code] [ASTExpression'] -> a
findTypeHandler [] typedefinition expressions = error ("could not find typehandler for " ++ show typedefinition)
findTypeHandler (currentTypeHandlerContainer : restTypeHandlerContainers) typeDefinition reference =
  case currentTypeHandlerContainer typeDefinition reference of
    Just typeHandlerContainer -> typeHandlerContainer
    Nothing -> findTypeHandler restTypeHandlerContainers typeDefinition reference

getStackEntries :: TypeHandler a => a -> ASTLeftHandSide -> [StackEntry a]
getStackEntries typeHandler ((ASTLeftHandSideVariable name)) = [(name, typeHandler)]

getTypeDefinitionFromExpression :: TypeHandler a => Stack a -> ASTExpression' -> ASTTypeDeclaration
getTypeDefinitionFromExpression stack (ASTExpressionVariable name) = error "no lookup implemented"
getTypeDefinitionFromExpression stack (ASTExpressionFunctionDeclaration parameters body) = ASTTypeDeclarationFunction [ASTTypeDeclarationGeneric (show index) | (parameter, index) <- zip parameters [0 ..]] (ASTTypeDeclarationGeneric (show (length parameters)))

getNestedTypeHandler :: TypeHandler a => [TypeHandlerContainer a] -> Stack a -> a -> ASTExpression -> [(ASTExpression', a)]
getNestedTypeHandler typehandlerContainers stack typeHandler [] = []
getNestedTypeHandler typehandlerContainers stack typeHandler (currentExpression@(ASTExpressionVariable variableName) : restNestedExpressions) =
  let Just (_, nestedTypeHandler) = find (\(property, _) -> property == variableName) (properties typeHandler typehandlerContainers)
   in (currentExpression, nestedTypeHandler) : getNestedTypeHandler typehandlerContainers stack nestedTypeHandler restNestedExpressions
