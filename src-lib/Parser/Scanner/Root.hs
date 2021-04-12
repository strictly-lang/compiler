module Parser.Scanner.Root (rootScanners) where

import Parser.Scanner.View (viewScanners)
import Parser.Util (parseLines)
import Types

rootScanner :: Scanner Root
rootScanner [] indentationLevel exprId = ([], exprId, [])
rootScanner indentedLines@((line, currentIndentation, currentLineValue) : restIndentedLines) indentationLevel exprId
  | currentIndentation == 0 && currentLineValue == "view" =
    let (children, exprId', restIndentedChildLines) = parseLines viewScanners restIndentedLines (currentIndentation + 1) (exprId + 1)
     in ( [Node exprId (View children)],
          exprId',
          restIndentedChildLines
        )
  | otherwise = ([], exprId, indentedLines)

rootScanners = [rootScanner]
