module Prelude.Javascript.Util where

import Control.Monad.State.Lazy (MonadState (state))
import Data.Char (toUpper)
import Data.Foldable (find)
import Parser.Types (ASTExpression, ASTExpression' (ASTExpressionVariable), ASTStatement (ASTExpression))
import Prelude.Javascript.Types
import TypeChecker.Main (findTypehandler)
import TypeChecker.Types (TypeHandlerContext (..), TypeValue (TypeValueByLiteral))

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
  typeHandler <- nestedExpression renderContext expression
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
render renderContext [] = do return (JavaScriptDomResult {create = [], update = [], dealloc = [], delete = []})

nestedExpression :: JavaScriptRenderContext -> ASTExpression -> AppStateMonad JavaScriptTypeHandler
nestedExpression renderContext (firstExpression : restExpression) = do
  let firstTypeHandler = case firstExpression of
        (ASTExpressionVariable variableName) ->
          let (Just (_, scope, typeHandler)) = find (\(variableName', _, _) -> variableName' == variableName) (runStack renderContext)
           in typeHandler
        firstExpression ->
          let Just typeHandler =
                findTypehandler
                  ( TypeHandlerContext
                      { TypeChecker.Types.runTypes = Prelude.Javascript.Types.runTypes renderContext
                      }
                  )
                  Nothing
                  (TypeValueByLiteral firstExpression)
           in typeHandler

  return
    firstTypeHandler
nestedExpression renderCOntext [] =
  error "empty expression"

getGetFreshExprId :: AppStateMonad Int
getGetFreshExprId =
  state
    ( \appState ->
        (runExpressionId appState, AppState {runExpressionId = runExpressionId appState + 1})
    )