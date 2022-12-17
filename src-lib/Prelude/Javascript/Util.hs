module Prelude.Javascript.Util where

import Control.Monad.State.Lazy (MonadState (state))
import Data.Char (toUpper)
import Data.Foldable (find)
import Data.Type.Equality (apply)
import Parser.Types (ASTExpression, ASTExpression' (ASTExpressionFunctionCall, ASTExpressionVariable), ASTLeftHandSide (ASTLeftHandSideRecord), ASTStatement (ASTExpression), ASTTypeDeclaration)
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
  nextResult <-
    render
      ( JavaScriptRenderContext
          { runParent = runParent renderContext,
            runSiblings = siblings result,
            Prelude.Javascript.Types.runTypes = Prelude.Javascript.Types.runTypes renderContext,
            runStack = runStack renderContext,
            runScope = runScope renderContext
          }
      )
      restSatements

  return
    ( JavaScriptDomResult
        { create = create result ++ create nextResult,
          update = update result ++ update nextResult,
          dealloc = dealloc result ++ dealloc nextResult,
          delete = delete result ++ delete nextResult,
          siblings = siblings nextResult
        }
    )
render renderContext [] = do
  return
    ( JavaScriptDomResult
        { create = [],
          update = [],
          dealloc = [],
          delete = [],
          siblings = []
        }
    )

nestedExpression :: JavaScriptRenderContext -> Maybe ASTTypeDeclaration -> [ASTExpression] -> AppStateMonad JavaScriptTypeHandler
nestedExpression renderContext typeDeclaration [firstExpressionPart : restExpressionPart] = do
  let firstTypeHandler = case firstExpressionPart of
        (ASTExpressionVariable variableName) ->
          case find (\(variableName', _, _) -> variableName' == variableName) (runStack renderContext) of
            (Just (_, scope, typeHandler)) -> typeHandler
            result -> error ("finding variable failed failed for " ++ variableName)
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
nestedExpression' renderContext javaScriptTypeHandler ((ASTExpressionFunctionCall parameter) : restExpression) = do
  result <- call javaScriptTypeHandler renderContext parameter
  nestedExpression' renderContext result restExpression
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

data Predecessor = PredecessorNone | PredecessorAlways [Code] | PredecessorMaybe [Code] [Code]

appendElement :: JavaScriptRenderContext -> [Property] -> [Code]
appendElement renderContext target =
  let result = appendElement' (runSiblings renderContext)
      parent' = propertyToCode (runParent renderContext)
      target' = propertyToCode target
      siblingsAfter sibling = sibling ++ [Ln ".after("] ++ target' ++ [Ln ");", Br]
      siblingsNone = parent' ++ [Ln ".append("] ++ target' ++ [Ln ");", Br]
   in case result of
        PredecessorAlways predecessor ->
          siblingsAfter predecessor
        PredecessorMaybe condition predecessor ->
          Ln "if ("
            : condition
            ++ [ Ln ") {",
                 Ind (siblingsAfter predecessor),
                 Ln "} else {",
                 Ind siblingsNone,
                 Ln "}",
                 Br
               ]
        PredecessorNone ->
          siblingsNone

appendElement' :: [Sibling] -> Predecessor
appendElement' [] = PredecessorNone
appendElement' ((SiblingAlways sibling) : restSiblings) = PredecessorAlways (propertyToCode sibling)
appendElement' ((SiblingCondition condition thenSiblings elseSiblings) : restSiblings) =
  let thenResult = appendElement' thenSiblings
      elseResult = appendElement' elseSiblings
   in case thenResult of
        PredecessorAlways thenResult' ->
          case elseResult of
            PredecessorAlways elseResult' ->
              PredecessorAlways
                ( Ln "("
                    : condition
                    ++ [Ln " ? "]
                    ++ thenResult'
                    ++ [Ln " : "]
                    ++ elseResult'
                    ++ [Ln ")"]
                )
            PredecessorMaybe elseCondition elseResult' ->
              PredecessorMaybe
                (Ln "(" : condition ++ [Ln " || "] ++ elseCondition ++ [Ln ")"])
                (Ln "(" : condition ++ [Ln " ? "] ++ thenResult' ++ Ln " : " : elseResult' ++ [Ln ")"])
            PredecessorNone ->
              PredecessorMaybe condition thenResult'
        PredecessorMaybe thenCondition thenResult' ->
          case elseResult of
            PredecessorAlways elseResult' ->
              PredecessorMaybe
                ( Ln "("
                    : condition
                    ++ [Ln " === false || "]
                    ++ thenCondition
                    ++ [Ln ")"]
                )
                ( Ln "("
                    : condition
                    ++ [Ln " ? "]
                    ++ thenResult'
                    ++ [Ln " ? "]
                    ++ elseResult'
                    ++ [Ln ")"]
                )
            PredecessorMaybe elseCondition elseResult' ->
              PredecessorMaybe
                ( Ln "(("
                    : condition
                    ++ [Ln " && "]
                    ++ thenCondition
                    ++ [Ln ") || ("]
                    ++ condition
                    ++ [Ln " === false && "]
                    ++ elseCondition
                    ++ [Ln "))"]
                )
                ( Ln "("
                    : condition
                    ++ [Ln " ? "]
                    ++ thenResult'
                    ++ [Ln " ? "]
                    ++ elseResult'
                    ++ [Ln ")"]
                )
            PredecessorNone ->
              PredecessorMaybe
                (Ln "(" : condition ++ [Ln " && "] ++ thenCondition ++ [Ln ")"])
                thenResult'
        PredecessorNone ->
          case elseResult of
            PredecessorAlways elseResult' ->
              PredecessorMaybe
                (Ln "(" : condition ++ [Ln " === false)"])
                elseResult'
            PredecessorMaybe elseCondition elseResult' ->
              PredecessorMaybe
                (Ln "(" : condition ++ [Ln " === false && "] ++ elseCondition ++ [Ln ")"])
                elseResult'
            PredecessorNone ->
              PredecessorNone