module Parser.Main (parse, getIndentedLines) where

import Parser.Scanner.Root (rootScanners)
import Parser.Util (parseLines)
import Types

parse content = parseRoot (getIndentedLines (lines content) 0)

getIndentedLines :: [String] -> Line -> [IndentedLine]
getIndentedLines [] _ = []
getIndentedLines (l : ls) line
  -- Empty line can be just be thrown away
  | indentationLevel == 0 && value == "" = getIndentedLines ls (line + 1)
  | otherwise = (line, indentationLevel, value) : getIndentedLines ls (line + 1)
  where
    (indentationLevel, value) = getIndentation l

getIndentation :: String -> (IndentationLevel, String)
getIndentation ('\t' : xs) =
  let (indent, value) = getIndentation xs
   in (indent + 1, value)
getIndentation xs = (0, xs)

parseRoot :: [IndentedLine] -> [Expr Root]
parseRoot indentedLines =
  let (rootNodes, _, restLines) = parseLines rootScanners indentedLines 0 0
   in rootNodes
