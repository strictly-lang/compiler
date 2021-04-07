module Parser.Main where

type Line = Int

type Column = Int

type IndentationLevel = Int

type IndentedLine = (Line, IndentationLevel, String)

type Option = [String]

type Position = (Line, Column)

type NodeName = String

newtype NodeTuple = NodeTuple (NodeName, Line, [Option], [Expr NodeTuple])

data Expr a = Node a | TypeError String Position Position

type Scanner a = [IndentedLine] -> IndentationLevel -> ([Expr a], [IndentedLine])

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
  let (rootNodes, restLines) = parseLines [genericScanner] indentedLines 0
   in rootNodes

parseLines :: [Scanner a] -> [IndentedLine] -> IndentationLevel -> ([Expr a], [IndentedLine])
parseLines _ [] _ = ([], [])
parseLines scanners indentedLines indentationLevel
  -- Current indent-level
  | currentIndentation == indentationLevel =
    let (nodes, restIndentedLines) = parseWithScanner scanners scanners indentedLines indentationLevel
        (siblingNodes, restSiblingLines) = parseLines scanners restIndentedLines indentationLevel
     in (nodes ++ siblingNodes, restSiblingLines)
  -- Outer indent level
  | currentIndentation < indentationLevel = ([], indentedLines)
  -- To much indented
  | currentIndentation > indentationLevel =
    let (nodes, restIndentedLines) = parseLines scanners (tail indentedLines) currentIndentation
     in ( nodes
            ++ [ TypeError
                   "Wrong indentation"
                   (line, indentationLevel)
                   (line, indentationLevel + length currentLineValue)
               ],
          restIndentedLines
        )
  where
    (line, currentIndentation, currentLineValue) = head indentedLines

parseWithScanner :: [Scanner a] -> [Scanner a] -> [IndentedLine] -> IndentationLevel -> ([Expr a], [IndentedLine])
-- No Scanners left
parseWithScanner [] _ (l : ls) indentationLevel =
  ( [ TypeError
        "Cant be parsed"
        (currentLine, indentationLevel)
        (currentLine, indentationLevel + length currentLineValue)
    ],
    ls
  )
  where
    (currentLine, currentIndentation, currentLineValue) = l
parseWithScanner (currentScanner : restCurrentScanners) allScanners indentedLines indentationLevel
  -- Scanner didnt take any lines, means scanner didnt care about lines
  | length scannedRestLines == length indentedLines = parseWithScanner restCurrentScanners allScanners indentedLines indentationLevel
  | otherwise = (scannedExprResult, scannedRestLines)
  where
    (scannedExprResult, scannedRestLines) = currentScanner indentedLines indentationLevel

genericScanner :: [IndentedLine] -> IndentationLevel -> ([Expr NodeTuple], [IndentedLine])
genericScanner indentedLines indentationLevel =
  let (children, restIndentedChildLines) =
        parseLines [genericScanner] (tail indentedLines) (currentIndentation + 1)
   in ( [Node (NodeTuple (currentLineValue, line, [], children))],
        restIndentedChildLines
      )
  where
    (line, currentIndentation, currentLineValue) = head indentedLines