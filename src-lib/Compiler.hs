module Compiler (compile) where

import Parser.Main (parse)
import Prelude.Javascript.Main (preludedTypehandlerContainer)
import Text.Megaparsec (errorBundlePretty)
import TypeChecker.Main (typecheck)
import WebcomponentEmitter.Main (emit)

compile :: String -> String -> Either String (String, String)
compile filePath fileContent = do
  ast <- parse fileContent
  typeCheckedResults <- typecheck preludedTypehandlerContainer [] ast
  emitResult <- emit preludedTypehandlerContainer filePath typeCheckedResults
  return (filePath, emitResult)

-- emit macros filePath ast
