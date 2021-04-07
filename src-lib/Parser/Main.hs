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

type Scanner a = [IndentedLine] -> ([Expr a], [IndentedLine])

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
  let (rootNodes, restLines) = parseLines indentedLines 0
   in rootNodes

parseLines :: [IndentedLine] -> IndentationLevel -> ([Expr NodeTuple], [IndentedLine])
parseLines [] _ = ([], [])
parseLines indentedLines indentLevel
  -- Current indent-level
  | currentIndentation == indentLevel =
    -- Children processing
    let (children, restIndentedChildLines) = parseLines (tail indentedLines) (currentIndentation + 1)
        -- Sibling Processing
        (siblings, restIndentedSiblingLines) = parseLines (tail restIndentedChildLines) currentIndentation
     in (Node (NodeTuple (currentLineValue, line, [], children)) : siblings, restIndentedSiblingLines)
  -- Outer indent level
  | currentIndentation < indentLevel = ([], indentedLines)
  -- To much indented
  | currentIndentation > indentLevel =
    let (nodes, restIndentedLines) = parseLines (tail indentedLines) currentIndentation
     in ( nodes
            ++ [ TypeError
                   "Wrong indentation"
                   (line, indentLevel)
                   (line, indentLevel + length currentLineValue)
               ],
          restIndentedLines
        )
  where
    (line, currentIndentation, currentLineValue) = head indentedLines

-- parseWithScanner :: a => [Scanner a] -> IndentationLevel -> Line -> [IndentedLine] -> ([Expr a], [IndentedLine])
-- -- No Scanners left
-- parseWithScanner [] indentLevel line (l:ls) = [TypeError "Cant be parsed"
--             (line, indentLevel)
--             (line, indentLevel + length currentLineValue)]
--   where
--     (currentIndentation, currentLineValue) = head indentedLines
-- parseWithScanner (scanner:scanners) indentLevel line indentedLines
--   -- Scanner didnt take any lines, means scanner didnt care about lines
--   | count scannedRestLines === count indentedLines = parseWithScanner scanners indentLevel line indentedLines
--   | otherwise = (scannedExprResult, scannedRestLines)
--   where
--     (scannedExprResult, scannedRestLines) = scanner indentedLines