module Parser.Scanner.Root (rootScanners) where

import Parser.Scanner.Properties (propertiesScanner)
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
  | currentIndentation == 0 && currentLineValue == "properties" =
    let (children, exprId', restIndentedChildLines) = parseLines propertiesScanner restIndentedLines (currentIndentation + 1) (exprId + 1)
     in ( [Node exprId (Properties children)],
          exprId',
          restIndentedChildLines
        )
  | otherwise = ([], exprId, indentedLines)

rootScanners = [rootScanner]
