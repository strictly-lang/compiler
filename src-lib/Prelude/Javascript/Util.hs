module Prelude.Javascript.Util where

import Control.Monad.State.Lazy (MonadState (state))
import Data.Char (toUpper)
import Data.Foldable (find)
import Parser.Types (ASTExpression, ASTExpression' (ASTExpressionVariable), ASTLeftHandSide (ASTLeftHandSideRecord), ASTStatement (ASTExpression), ASTTypeDeclaration)
import Prelude.Javascript.Types
import TypeChecker.Main (findTypehandler)
import TypeChecker.Types (TypeHandler, TypeHandlerContext (..), TypeValue (TypeValueByLiteral))

codeToString :: Int -> Bool -> [Code] -> String
codeToString indentationLevel first [] = ""
codeToString indentationLevel first (Ind nestedCode : restCode) =
  "\n"
    ++ codeToString (indentationLevel + 1) True nestedCode
    ++ "\n"
    ++ codeToString indentationLevel True restCode
codeToString indentationLevel first (Ln code : restCode)
  | first = replicate indentationLevel '\t' ++ code'
  | otherwise = code'
  where
    code' = code ++ codeToString indentationLevel False restCode
codeToString indentationLevel first (Br : restCode) = '\n' : codeToString indentationLevel True restCode

removeFileExtension :: String -> String
removeFileExtension p = take (length p - length ".sly") p

slashToDash :: String -> String
slashToDash [] = []
slashToDash ('/' : ps) = '-' : slashToDash ps
slashToDash (p : ps) = p : slashToDash ps

slashToCamelCase :: String -> String
slashToCamelCase (p : ps) = toUpper p : slashToCamelCase' ps

slashToCamelCase' :: String -> String
slashToCamelCase' [] = []
slashToCamelCase' ('/' : p : ps) = toUpper p : slashToCamelCase' ps
slashToCamelCase' (p : ps) = p : slashToCamelCase' ps

render :: JavaScriptRenderContext -> [ASTStatement] -> AppStateMonad JavaScriptDomResult
render renderContext ((ASTExpression expression) : restSatements) = do
  typeHandler <- nestedExpression renderContext Nothing [expression]
  result <- getDom typeHandler renderContext
  nextResult <- render renderContext restSatements

  return
    ( JavaScriptDomResult
        { create = create result ++ create nextResult,
          update = update result ++ update nextResult,
          dealloc = dealloc result ++ dealloc nextResult,
          delete = delete result ++ delete nextResult
        }
    )
render renderContext [] = do
  return
    ( JavaScriptDomResult
        { create = [],
          update = [],
          dealloc = [],
          delete = []
        }
    )

nestedExpression :: JavaScriptRenderContext -> Maybe ASTTypeDeclaration -> [ASTExpression] -> AppStateMonad JavaScriptTypeHandler
nestedExpression renderContext typeDeclaration [firstExpressionPart : restExpressionPart] = do
  let firstTypeHandler = case firstExpressionPart of
        (ASTExpressionVariable variableName) ->
          let (Just (_, scope, typeHandler)) = find (\(variableName', _, _) -> variableName' == variableName) (runStack renderContext)
           in typeHandler
        firstExpressionPart ->
          let Just typeHandler =
                findTypehandler
                  ( TypeHandlerContext
                      { TypeChecker.Types.runTypes = Prelude.Javascript.Types.runTypes renderContext
                      }
                  )
                  typeDeclaration
                  [TypeValueByLiteral firstExpressionPart]
           in typeHandler

  nestedExpression' renderContext firstTypeHandler restExpressionPart
nestedExpression renderContext typeDeclaration _ =
  error "empty expression"

nestedExpression' :: JavaScriptRenderContext -> JavaScriptTypeHandler -> ASTExpression -> AppStateMonad JavaScriptTypeHandler
nestedExpression' renderContext javaScriptTypeHandler ((ASTExpressionVariable variableName) : restExpression) = do
  result <- destructure javaScriptTypeHandler renderContext (ASTLeftHandSideRecord [(variableName, Nothing)])
  case result of
    [((_, _, nestedTypeHandler), [])] ->
      nestedExpression' renderContext nestedTypeHandler restExpression
    _ -> error "destructuring failed"
nestedExpression' _ javaScriptTypeHandler [] = do return javaScriptTypeHandler
nestedExpression' _ _ (expression : restExpression) = error ("nesting expression with that doesnt work " ++ show expression)

getGetFreshExprId :: AppStateMonad Int
getGetFreshExprId =
  state
    ( \appState ->
        (runExpressionId appState, AppState {runExpressionId = runExpressionId appState + 1})
    )

propertyToCode :: [Property] -> [Code]
propertyToCode ((DotNotation firstNotation) : restNotations) = [Ln firstNotation] ++ propertyToCode' restNotations

propertyToCode' :: [Property] -> [Code]
propertyToCode' ((DotNotation currentNotation) : restNotations) = Ln ("." ++ currentNotation) : propertyToCode' restNotations
propertyToCode' ((BracketNotation currentNotation) : restNotations) = Ln ("[" ++ currentNotation ++ "]") : propertyToCode' restNotations
propertyToCode' [] = []