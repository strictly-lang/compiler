module Compiler (compile) where

import Parser.Main (parse)
import Prelude.Javascript.Main (preludedTypehandlerContainer)
import Text.Megaparsec (errorBundlePretty)
import TypeChecker.Main (typecheck)

compile :: String -> String -> Either String (String, String)
compile filePath fileContent = do
  ast <- parse fileContent
  typeCheckedResults <- typecheck preludedTypehandlerContainer ast
  return (filePath, show typeCheckedResults)

-- emit macros filePath ast
