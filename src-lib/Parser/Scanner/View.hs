module Parser.Scanner.View where

import Parser.Util
import Types

hostScanner :: Scanner View
hostScanner [] indentationLevel exprId = ([], exprId, [])
hostScanner indentedLines indentationLevel exprId =
  let (children, exprId', restIndentedChildLines) = parseLines viewScanners (tail indentedLines) (currentIndentation + 1) (exprId + 1)
   in ( [Node exprId (Host currentLineValue children [])],
        exprId',
        restIndentedChildLines
      )
  where
    (line, currentIndentation, currentLineValue) = head indentedLines

viewScanners = [hostScanner]