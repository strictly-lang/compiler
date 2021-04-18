module Parser.Util where

import Types

parseLines :: [Scanner a] -> [[Token]] -> IndentationLevel -> ExprId -> ([Node a], ExprId, [[Token]])
parseLines _ [] _ exprId = ([], exprId, [])
parseLines scanners indentedLines@(((Token currentPosition (Indentation currentIndentation)) : currentLineTokens) : restIndentedLines) indentationLevel exprId
  -- Current indent-level
  | currentIndentation == indentationLevel =
    let (nodes, exprId', indentedLines') = parseWithScanner scanners scanners (currentLineTokens : restIndentedLines) indentationLevel exprId
        (siblingNodes, exprId'', indentedLines'') = parseLines scanners indentedLines' indentationLevel exprId'
     in (nodes ++ siblingNodes, exprId'', indentedLines'')
  -- Outer indent level
  | currentIndentation < indentationLevel = ([], exprId, indentedLines)
  -- To much indented
  | currentIndentation > indentationLevel =
    let (_, _, indentedLines') = parseLines scanners restIndentedLines (currentIndentation + 1) exprId
     in ( [ SyntaxError
              "Wrong indentation"
              currentPosition
              currentPosition
          ],
          exprId,
          indentedLines'
        )

parseWithScanner :: [Scanner a] -> [Scanner a] -> [[Token]] -> IndentationLevel -> ExprId -> ([Node a], ExprId, [[Token]])
-- No Scanners left
parseWithScanner [] _ indentedLines@(((Token (currentLine, _) (Indentation currentIndentation)) : currentLineTokens) : restIndentedLines) indentationLevel exprId =
  ( [ SyntaxError
        "I don't know what to do with this"
        (currentLine, indentationLevel)
        (currentLine, indentationLevel)
    ],
    exprId,
    restIndentedLines
  )
parseWithScanner (currentScanner : restCurrentScanners) allScanners indentedLines indentationLevel exprId
  -- Scanner didnt consume any lines, means scanner didnt care about lines
  | length scannedRestLines == length indentedLines = parseWithScanner restCurrentScanners allScanners indentedLines indentationLevel exprId
  -- Scanner did consume lines, therefore scanned results should be used
  | otherwise = (scannedExprResult, exprId', scannedRestLines)
  where
    (scannedExprResult, exprId', scannedRestLines) = currentScanner indentedLines indentationLevel exprId
