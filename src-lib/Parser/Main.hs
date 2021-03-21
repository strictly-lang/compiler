module Parser.Main where

type Line = Int
type Column = Int
type IndentationLevel = Int
type IndentedLine = (Int, String)
type Option = [String]
type Range = (Line, Column, Column)
type NodeName = String
type ErrorTuple = (String, Range)
type NodeTuple = (NodeName, Line, [Option], [MaybeNode])
data MaybeNode = Node NodeTuple | Error ErrorTuple

parse content = parseRoot( map getIndentation (lines content))

getIndentation :: String -> IndentedLine
getIndentation ('\t':xs) =
    let (indent, value) = getIndentation xs
    in (indent + 1 , value)
getIndentation xs = (0, xs)

parseRoot :: [IndentedLine] -> [MaybeNode]
parseRoot indentedLines =
    let (rootNodes, restLines) = parseIndent 0 0 indentedLines
    in rootNodes

parseIndent :: IndentationLevel -> Line -> [IndentedLine] -> ([MaybeNode], [IndentedLine])
parseIndent _ _ [] = ([], [])
parseIndent indentLevel line indentedLines
    | currentIndentation == indentLevel =
        let (children, restIndentedChildLines) = parseIndent (currentIndentation + 1) (line + 1) (tail indentedLines)
            (siblings, restIndentedSiblingLines) = parseIndent (currentIndentation + 1) (line + length indentedLines - length restIndentedChildLines) (tail restIndentedChildLines)
        in (Node (currentLineValue, line, [], children) : siblings, restIndentedSiblingLines)
    | currentIndentation < indentLevel = ([] , indentedLines)
    | currentIndentation > indentLevel =
        let (nodes, restIndentedLines) = parseIndent (currentIndentation + 1) (line + 1 )(tail indentedLines)
        in (Error ("Wrong indentation", (line, indentLevel, indentLevel + length currentLineValue)) : nodes, restIndentedLines)
    where
        (currentIndentation, currentLineValue) = head indentedLines