module Compiler (compile) where

import Parser.Main (parse)
import Prelude.Javascript.Main (macros)
import Prelude.Main (emit)
import Text.Megaparsec (errorBundlePretty)

compile :: String -> String -> Either String (String, String)
compile filePath fileContent = do
  ast <- parse fileContent
  emit macros filePath ast
