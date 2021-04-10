module Parser.Main where

import Types

type IndentationLevel = Int

type IndentedLine = (Line, IndentationLevel, String)

type Scanner a = [IndentedLine] -> IndentationLevel -> ExprId -> ([Expr a], ExprId, [IndentedLine])

parse content = parseRoot (getIndentedLines (lines content) 0)

getIndentedLines :: [String] -> Line -> [IndentedLine]
getIndentedLines [] _ = []
getIndentedLines (l : ls) line
  -- Empty line can be just be thrown away
  | indentationLevel == 0 && value == "" = getIndentedLines ls (line + 1)
  | otherwise = (indentationLevel, line, value) : getIndentedLines ls (line + 1)
  where
    (indentationLevel, value) = getIndentation l

getIndentation :: String -> (IndentationLevel, String)
getIndentation ('\t' : xs) =
  let (indent, value) = getIndentation xs
   in (indent + 1, value)
getIndentation xs = (0, xs)

parseRoot :: [IndentedLine] -> [Expr NodeTuple]
parseRoot indentedLines =
  let (rootNodes, _, restLines) = parseLines [genericScanner] indentedLines 0 0
   in rootNodes

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

genericScanner :: [IndentedLine] -> IndentationLevel -> ExprId -> ([Expr NodeTuple], ExprId, [IndentedLine])
genericScanner indentedLines indentationLevel exprId =
  let exprId' = exprId' + 1
      (children, exprId'', restIndentedChildLines) =
        parseLines [genericScanner] (tail indentedLines) (currentIndentation + 1) exprId'
   in ( [Node exprId' (NodeTuple (currentLineValue, line, [], children))],
        exprId'',
        restIndentedChildLines
      )
  where
    (line, currentIndentation, currentLineValue) = head indentedLines