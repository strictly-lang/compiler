module WebcomponentEmitter.Util where

import Control.Monad.State.Lazy (MonadState (state))
import Data.Char (toUpper)
import WebcomponentEmitter.Types

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
codeToString indentationLevel first ((Inl lines) : restCode) =
  codeToString indentationLevel first lines ++ codeToString indentationLevel False restCode
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

getGetFreshExprId :: AppStateMonad Int
getGetFreshExprId =
  state
    ( \appState ->
        (runExpressionId appState, AppState {runExpressionId = runExpressionId appState + 1})
    )

propertyToCode :: [Property] -> [Code]
propertyToCode ((DotNotation firstNotation) : restNotations) = Ln firstNotation : propertyToCode' restNotations

propertyToCode' :: [Property] -> [Code]
propertyToCode' ((DotNotation currentNotation) : restNotations) = Ln ("." ++ currentNotation) : propertyToCode' restNotations
propertyToCode' ((BracketNotation currentNotation) : restNotations) = Ln ("[" ++ currentNotation ++ "]") : propertyToCode' restNotations
propertyToCode' [] = []
