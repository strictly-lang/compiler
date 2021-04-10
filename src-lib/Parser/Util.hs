module Parser.Util where

import Types

parseLines :: [Scanner a] -> [IndentedLine] -> IndentationLevel -> ExprId -> ([Expr a], ExprId, [IndentedLine])
parseLines _ [] _ exprId = ([], exprId, [])
parseLines scanners indentedLines@((line, currentIndentation, currentLineValue) : restIndentedLines) indentationLevel exprId
  -- Current indent-level
  | currentIndentation == indentationLevel =
    let (nodes, exprId', indentedLines') = parseWithScanner scanners scanners indentedLines indentationLevel exprId
        (siblingNodes, exprId'', indentedLines'') = parseLines scanners indentedLines' indentationLevel exprId'
     in (nodes ++ siblingNodes, exprId'', indentedLines'')
  -- Outer indent level
  | currentIndentation < indentationLevel = ([], exprId, indentedLines)
  -- To much indented
  | currentIndentation > indentationLevel =
    let (nodes, exprId', indentedLines') = parseLines scanners restIndentedLines currentIndentation exprId
     in ( nodes
            ++ [ SyntaxError
                   "Wrong indentation"
                   (line, indentationLevel)
                   (line, indentationLevel + length currentLineValue)
               ],
          exprId',
          indentedLines'
        )

parseWithScanner :: [Scanner a] -> [Scanner a] -> [IndentedLine] -> IndentationLevel -> ExprId -> ([Expr a], ExprId, [IndentedLine])
-- No Scanners left
parseWithScanner [] _ (l : ls) indentationLevel exprId =
  ( [ SyntaxError
        "I don't know what to do with this"
        (currentLine, indentationLevel)
        (currentLine, indentationLevel + length currentLineValue)
    ],
    exprId,
    ls
  )
  where
    (currentLine, currentIndentation, currentLineValue) = l
parseWithScanner (currentScanner : restCurrentScanners) allScanners indentedLines indentationLevel exprId
  -- Scanner didnt consume any lines, means scanner didnt care about lines
  | length scannedRestLines == length indentedLines = parseWithScanner restCurrentScanners allScanners indentedLines indentationLevel exprId'
  | otherwise = (scannedExprResult, exprId, scannedRestLines)
  where
    (scannedExprResult, exprId', scannedRestLines) = currentScanner indentedLines indentationLevel exprId
