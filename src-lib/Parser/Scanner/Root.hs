module Parser.Scanner.Root where

import Parser.Scanner.View
import Parser.Util
import Types

viewScanner :: Scanner Root
viewScanner [] indentationLevel exprId = ([], exprId, [])
viewScanner indentedLines indentationLevel exprId
  | currentIndentation == 0 && currentLineValue == "view" =
    let (children, exprId', restIndentedChildLines) = parseLines viewScanners (tail indentedLines) (currentIndentation + 1) (exprId + 1)
     in ( [Node exprId (View children)],
          exprId',
          restIndentedChildLines
        )
  where
    (line, currentIndentation, currentLineValue) = head indentedLines

rootScanners = [viewScanner]