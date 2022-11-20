module Parser.Main where

import Control.Monad.State.Strict (runState)
import Data.Void (Void)
import Parser.Root (rootParser)
import Parser.Types
import Text.Megaparsec (State, eof, many, parse)
import Text.Megaparsec.Char (eol)
import Text.Megaparsec.Error

parse :: String -> Either String AST
parse = do
  result <- Text.Megaparsec.parse parseRoot' ""
  case result of
    Left parseError ->
      return (Left (errorBundlePretty parseError))
    Right ast ->
      return (Right ast)

parseRoot' :: Parser AST
parseRoot' = do
  groupRoot <$> many (many eol *> (rootParser <* many eol)) <* eof

groupRoot :: [ASTRootNodeUngrouped] -> AST
groupRoot ((ASTRootNodeUngroupedAlgebraicDataTypeDeclaration name values) : nrs) = ASTRootNodeGroupedAlgebraicDataTypeDeclaration name values : groupRoot nrs
groupRoot ((ASTRootNodeUngroupedMacro macroName) : nrs) =
  case groupRoot nrs of
    (ASTRootNodeGroupedAssignment assignmentName macro typeDeclaration expressions) : ngrs ->
      ASTRootNodeGroupedAssignment assignmentName (Just macroName) typeDeclaration expressions : ngrs
    _ ->
      error "could not find assignment for macro"
groupRoot ((ASTRootNodeUngroupedTypeAssignment assignmentName typeDeclaration) : nrs) =
  case groupRoot nrs of
    nextRootAssignment@(ASTRootNodeGroupedAssignment assignmentName' macro typeDeclaration' expressions) : ngrs ->
      if assignmentName' == assignmentName
        then case macro of
          Just _ ->
            error "cant have macro after typedeclaration"
          Nothing ->
            case typeDeclaration' of
              Just _ ->
                error "cant have two typedeclarations"
              Nothing ->
                ASTRootNodeGroupedAssignment assignmentName Nothing (Just typeDeclaration) expressions : ngrs
        else ASTRootNodeGroupedAssignment assignmentName Nothing (Just typeDeclaration) expressions : nextRootAssignment : ngrs
    ngrs ->
      ASTRootNodeGroupedAssignment assignmentName Nothing (Just typeDeclaration) [] : ngrs
groupRoot ((ASTRootNodeUngroupedAssignment assignmentName expression) : nrs) =
  case groupRoot nrs of
    nextRootAssignment@(ASTRootNodeGroupedAssignment assignmentName' macro typeDeclaration' expressions) : ngrs ->
      if assignmentName' == assignmentName
        then case macro of
          Just _ ->
            error "cant have macro after typedeclaration"
          Nothing ->
            case typeDeclaration' of
              Just _ ->
                error "cant have two typedeclarations"
              Nothing ->
                ASTRootNodeGroupedAssignment assignmentName Nothing Nothing expressions : ngrs
        else ASTRootNodeGroupedAssignment assignmentName Nothing Nothing (expression : expressions) : nextRootAssignment : ngrs
    ngrs ->
      ASTRootNodeGroupedAssignment assignmentName Nothing Nothing [expression] : ngrs
groupRoot [] = []