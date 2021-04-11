module Parser.Scanner.View where

import Parser.Util (parseLines)
import Types

viewScanner :: Scanner View
viewScanner [] indentationLevel exprId = ([], exprId, [])
viewScanner (indentedLines@(line, currentIndentation, '"' : currentLineValue) : restIndentedLines) indentationLevel exprId =
  (textNodes, exprId', restIndentedLines)
  where
    (textNodes, exprId') = staticTextParserWrapper currentLineValue exprId
viewScanner (indentedLines@(line, currentIndentation, currentLineValue) : restIndentedLines) indentationLevel exprId =
  let (children, exprId', indentedLines') = parseLines viewScanners restIndentedLines (currentIndentation + 1) (exprId + 1)
      nodeName = head (words currentLineValue)
   in ( [Node exprId (Host nodeName children [])],
        exprId',
        indentedLines'
      )

viewScanners = [viewScanner]

staticTextParserWrapper :: String -> ExprId -> ([Expr View], ExprId)
-- Do a syntax-error here, you cant end without a closing "
staticTextParserWrapper [] exprId = ([], exprId)
staticTextParserWrapper ['"'] exprId = ([], exprId)
staticTextParserWrapper ('$' : '{' : rest) exprId =
  (Node exprId (DynamicText dynamicText) : expr', exprId')
  where
    (dynamicText, rest') = dynamicTextParser rest
    (expr', exprId') = staticTextParserWrapper rest' (exprId + 1)
staticTextParserWrapper cs exprId = (Node exprId (StaticText staticText) : expr', exprId')
  where
    (staticText, rest) = staticTextParser cs
    (expr', exprId') = staticTextParserWrapper rest (exprId + 1)

dynamicTextParser :: String -> (String, String)
-- Do a syntax-error here, you cant end without a closing }
dynamicTextParser [] = ("", "")
dynamicTextParser ('}' : rest) = ("", rest)
dynamicTextParser (c : cs) = (c : cs', rest)
  where
    (cs', rest) = dynamicTextParser cs

staticTextParser :: String -> (String, String)
-- Do a syntax-error here, you cant end without a closing }
staticTextParser [] = ("", "")
staticTextParser ['"'] = ("", "")
staticTextParser all@('$' : '{' : rest) = ("", all)
staticTextParser (c : cs) = (c : cs', rest)
  where
    (cs', rest) = staticTextParser cs
