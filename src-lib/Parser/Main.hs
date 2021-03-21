module Parser.Main where

type LineCount = Int;
type IndentedLine = (Int, String);
type Option = [String];
type Position = (Int, Int);
type NodeName = String;
type ErrorTuple = (String, Position);
type NodeTuple = (NodeName, LineCount, [Option], [MaybeNode])
data MaybeNode = Node NodeTuple | Error ErrorTuple;

parse content = parseRoot( map getIndentation (lines content))

getIndentation :: String -> IndentedLine
getIndentation ('\t':xs) =
    let (indent, value) = getIndentation xs;
    in (indent + 1 , value)
getIndentation xs = (0, xs);

parseRoot :: [IndentedLine] -> [MaybeNode];
parseRoot indentedLines = [Node ("foo", 1, [], [])]


parseIndent :: LineCount -> [IndentedLine] -> ([MaybeNode], [IndentedLine])
parseIndent _ [] = ([], [])
parseIndent indentLevel indentedLines
    | currentIndentation == indentLevel =
        let (children, restIndentedChildLines) = parseIndent (currentIndentation + 1) (tail indentedLines);
            (siblings, restIndentedSiblingLines) = parseIndent (currentIndentation + 1) (tail restIndentedChildLines);
        in (Node (currentLineValue, 0, [], children) : siblings, restIndentedSiblingLines)
    | currentIndentation < indentLevel = ([] , indentedLines)
    | currentIndentation > indentLevel =
        let (nodes, restIndentedLines) = parseIndent (currentIndentation + 1) (tail indentedLines);
        in (Error ("Wrong indentation", (indentLevel, 0)) : nodes, restIndentedLines)

    where
        (currentIndentation, currentLineValue) = head indentedLines;