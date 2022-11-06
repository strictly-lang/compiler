module Compiler (compile) where

import Parser.Main (parse)
import Text.Megaparsec (errorBundlePretty)

compile :: String -> String -> Either String (String, String)
compile filePath fileContent = case parse fileContent of
  Right ast ->
    Right (filePath, show ast)
  Left parseError ->
    Left (errorBundlePretty parseError)
