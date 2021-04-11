module Parser.Scanner.View where

import Parser.Util ( parseLines )
import Types

viewScanner :: Scanner View
viewScanner [] indentationLevel exprId = ([], exprId, [])
viewScanner (indentedLines@(line, currentIndentation, '"':currentLineValue):restIndentedLines) indentationLevel exprId
  | lastCharacter == '"' =
      ( [Node exprId (StaticText restLineValue)],
          exprId + 1,
          restIndentedLines
        )
  | otherwise = (
      [SyntaxError
        "I expected an \" here"
        (line, currentIndentation + (length  currentLineValue - 1))
        (line, currentIndentation + length  currentLineValue)
      ],
      exprId,
      restIndentedLines
    )
  where
    lastCharacter:reverseRestLineValue = reverse currentLineValue
    restLineValue = reverse reverseRestLineValue
viewScanner (indentedLines@(line, currentIndentation, currentLineValue):restIndentedLines) indentationLevel exprId =
  let (children, exprId', indentedLines') = parseLines viewScanners restIndentedLines (currentIndentation + 1) (exprId + 1)
      nodeName = head (words currentLineValue)
   in ( [Node exprId (Host nodeName children [])],
        exprId',
        indentedLines'
      )

viewScanners = [viewScanner]
