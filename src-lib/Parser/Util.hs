module Parser.Util where

import Types

parseLines :: [Scanner a] -> [IndentedLine] -> IndentationLevel -> ExprId -> ([Expr a], ExprId, [IndentedLine])
parseLines _ [] _ exprId = ([], exprId, [])
parseLines scanners indentedLines indentationLevel exprId
  -- Current indent-level
  | currentIndentation == indentationLevel =
    let (nodes, exprId', restIndentedLines) = parseWithScanner scanners scanners indentedLines indentationLevel exprId
        (siblingNodes, exprId'', restSiblingLines) = parseLines scanners restIndentedLines indentationLevel exprId'
     in (nodes ++ siblingNodes, exprId'', restSiblingLines)
  -- Outer indent level
  | currentIndentation < indentationLevel = ([], exprId, indentedLines)
  -- To much indented
  | currentIndentation > indentationLevel =
    let (nodes, exprId', restIndentedLines) = parseLines scanners (tail indentedLines) currentIndentation exprId
     in ( nodes
            ++ [ SyntaxError
                   "Wrong indentation"
                   (line, indentationLevel)
                   (line, indentationLevel + length currentLineValue)
               ],
          exprId',
          restIndentedLines
        )
  where
    (line, currentIndentation, currentLineValue) = head indentedLines

parseWithScanner :: [Scanner a] -> [Scanner a] -> [IndentedLine] -> IndentationLevel -> ExprId -> ([Expr a], ExprId, [IndentedLine])
-- No Scanners left
parseWithScanner [] _ (l : ls) indentationLevel exprId =
  ( [ SyntaxError
        "Cant be parsed"
        (currentLine, indentationLevel)
        (currentLine, indentationLevel + length currentLineValue)
    ],
    exprId,
    ls
  )
  where
    (currentLine, currentIndentation, currentLineValue) = l
parseWithScanner (currentScanner : restCurrentScanners) allScanners indentedLines indentationLevel exprId
  -- Scanner didnt take any lines, means scanner didnt care about lines
  | length scannedRestLines == length indentedLines = parseWithScanner restCurrentScanners allScanners indentedLines indentationLevel exprId'
  | otherwise = (scannedExprResult, exprId', scannedRestLines)
  where
    (scannedExprResult, exprId', scannedRestLines) = currentScanner indentedLines indentationLevel exprId
