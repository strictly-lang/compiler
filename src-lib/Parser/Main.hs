module Parser.Main where

type Line = Int

type Column = Int

type IndentationLevel = Int

type IndentedLine = (Int, String)

type Option = [String]

type Position = (Line, Column)

type NodeName = String

type NodeTuple = (NodeName, Line, [Option], [Expr])

data Expr = Node NodeTuple | TypeError String Position Position

type Parser a = [IndentedLine] -> ([a], [IndentedLine]);

parse content = parseRoot (map getIndentation (lines content))

getIndentation :: String -> IndentedLine
getIndentation ('\t' : xs) =
  let (indent, value) = getIndentation xs
   in (indent + 1, value)
getIndentation xs = (0, xs)

parseRoot :: [IndentedLine] -> [Expr]
parseRoot indentedLines =
  let (rootNodes, restLines) = parseLines 0 0 indentedLines
   in rootNodes

parseLines :: IndentationLevel -> Line -> [IndentedLine] -> ([Expr], [IndentedLine])
parseLines _ _ [] = ([], [])
parseLines indentLevel line indentedLines
  -- Empty Line
  | currentLineValue == "" = parseLines indentLevel (line + 1) (tail indentedLines)
  -- Current indent-level
  | currentIndentation == indentLevel =
    -- Children processing
    let (children, restIndentedChildLines) = parseLines (currentIndentation + 1) (line + 1) (tail indentedLines)
        -- Sibling Processing
        (siblings, restIndentedSiblingLines) = parseLines currentIndentation (line + length indentedLines - length restIndentedChildLines) (tail restIndentedChildLines)
     in (Node (currentLineValue, line, [], children) : siblings, restIndentedSiblingLines)
  -- Outer indent level
  | currentIndentation < indentLevel = ([], indentedLines)
  -- To much indented
  | currentIndentation > indentLevel =
    let (nodes, restIndentedLines) = parseLines currentIndentation (line + 1) (tail indentedLines)
     in ( nodes ++ [TypeError
            "Wrong indentation"
            (line, indentLevel)
            (line, indentLevel + length currentLineValue)],
          restIndentedLines
        )
  where
    (currentIndentation, currentLineValue) = head indentedLines