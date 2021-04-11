module Parser.Scanner.View where

import Parser.Util ( parseLines )
import Types

hostScanner :: Scanner View
hostScanner [] indentationLevel exprId = ([], exprId, [])
hostScanner (indentedLines@(line, currentIndentation, currentLineValue):restIndentedLines) indentationLevel exprId =
  let (children, exprId', indentedLines') = parseLines viewScanners restIndentedLines (currentIndentation + 1) (exprId + 1)
      nodeName = head (words currentLineValue)
   in ( [Node exprId (Host nodeName children [])],
        exprId',
        indentedLines'
      )

viewScanners = [hostScanner]
