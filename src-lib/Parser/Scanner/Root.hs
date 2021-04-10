module Parser.Scanner.Root where

import Parser.Scanner.View (viewScanners)
import Parser.Util (parseLines)
import Types

viewScanner :: Scanner Root
viewScanner [] indentationLevel exprId = ([], exprId, [])
viewScanner indentedLines@((line, currentIndentation, currentLineValue) : restIndentedLines) indentationLevel exprId
  | currentIndentation == 0 && currentLineValue == "view" =
    let (children, exprId', restIndentedChildLines) = parseLines viewScanners restIndentedLines (currentIndentation + 1) (exprId + 1)
     in ( [Node exprId (View children)],
          exprId',
          restIndentedChildLines
        )
  | currentIndentation == 0 && currentLineValue == "properties" =
    ( [Node exprId (Properties [])],
      exprId + 1,
      restIndentedLines
    )
  | otherwise = ([], exprId, indentedLines)

rootScanners = [viewScanner]
