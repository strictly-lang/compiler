module Parser.Main (parse) where

import Parser.Scanner.Root (rootScanners)
import Parser.Util (parseLines)
import Types

parse :: [[Token]] -> [Node Root]
parse indentedLines =
  let (rootNodes, _, restLines) = parseLines rootScanners indentedLines 0 0
   in rootNodes
