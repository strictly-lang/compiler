module Prelude.Javascript.Util where

import Data.Char (toUpper)
import Parser.Types (ASTExpression, ASTStatement (ASTExpression))
import Prelude.Javascript.Types (Code (..), JavaScriptRenderContext (runTypes), JavaScriptTypeHandler, getDom)
import TypeChecker.Main (findTypehandler)
import TypeChecker.Types (TypeHandlerContext (..))

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

render :: JavaScriptRenderContext -> [ASTStatement] -> [Code]
render renderContext ((ASTExpression expression) : restSatements) =
  let Just typeHandler = findTypehandler (TypeHandlerContext {TypeChecker.Types.runTypes = Prelude.Javascript.Types.runTypes renderContext}) expression
   in (getDom typeHandler renderContext) ++ [Br] ++ render renderContext restSatements
render renderContext [] = []
