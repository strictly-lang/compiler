module Parser.Main where

type Line = Int

type Column = Int

type IndentationLevel = Int

type IndentedLine = (Int, String)

type Option = [String]

type Position = (Line, Column)

type NodeName = String

type NodeTuple = (NodeName, Line, [Option], [Expr])

newtype Expr = Node NodeTuple

type Parser a = [IndentedLine] -> ([a], [IndentedLine], [Error]);

data Error = TypeError String Position Position


parse content = parseRoot (map getIndentation (lines content))

getIndentation :: String -> IndentedLine
getIndentation ('\t' : xs) =
  let (indent, value) = getIndentation xs
   in (indent + 1, value)
getIndentation xs = (0, xs)

parseRoot :: [IndentedLine] -> [Expr]
parseRoot indentedLines =
  let (rootNodes, restLines, errors) = parseLines 0 0 indentedLines
   in rootNodes

parseLines :: IndentationLevel -> Line -> [IndentedLine] -> ([Expr], [IndentedLine], [Error])
parseLines _ _ [] = ([], [], [])
parseLines indentLevel line indentedLines
  -- Empty Line
  | currentLineValue == "" = parseLines indentLevel (line + 1) (tail indentedLines)
  -- Current indent-level
  | currentIndentation == indentLevel =
    -- Children processing
    let (children, restIndentedChildLines, childrenErrors) = parseLines (currentIndentation + 1) (line + 1) (tail indentedLines)
        -- Sibling Processing
        (siblings, restIndentedSiblingLines, siblingErrors) = parseLines currentIndentation (line + length indentedLines - length restIndentedChildLines) (tail restIndentedChildLines)
     in (Node (currentLineValue, line, [], children) : siblings, restIndentedSiblingLines, childrenErrors ++ siblingErrors)
  -- Outer indent level
  | currentIndentation < indentLevel = ([], indentedLines, [])
  -- To much indented
  | currentIndentation > indentLevel =
    let (nodes, restIndentedLines, siblingErrors) = parseLines currentIndentation (line + 1) (tail indentedLines)
     in ( nodes,
          restIndentedLines,
          TypeError
            "Wrong indentation"
            (line, indentLevel)
            (line, indentLevel + length currentLineValue) :
          siblingErrors
        )
  where
    (currentIndentation, currentLineValue) = head indentedLines