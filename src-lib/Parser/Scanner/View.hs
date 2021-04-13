module Parser.Scanner.View where

import Parser.Util (parseLines)
import Types

viewScanner :: Scanner View
viewScanner [] indentationLevel exprId = ([], exprId, [])
viewScanner ((line, currentIndentation, '"' : currentLineValue) : restIndentedLines) indentationLevel exprId =
  let (textNodes, exprId') = staticTextParserWrapper currentLineValue exprId
   in (textNodes, exprId', restIndentedLines)
viewScanner ((line, currentIndentation, '#' : currentLineValue) : restIndentedLines) indentationLevel exprId
  | head currentLineValueWords == "if" =
    let (positiveChildren, exprId', indentedLines') = parseLines viewScanners restIndentedLines (currentIndentation + 1) (exprId + 1)
        (negativeChildren, exprId'', indentedLines'') = elseScanner indentedLines' currentIndentation exprId'
     in ( [Node exprId (Condition (Expr (currentLineValueWords !! 1)) positiveChildren negativeChildren)],
          exprId'',
          indentedLines''
        )
  where
    currentLineValueWords = words currentLineValue
viewScanner ((line, currentIndentation, currentLineValue) : restIndentedLines) indentationLevel exprId =
  let (children, exprId', indentedLines') = parseLines viewScanners restIndentedLines (currentIndentation + 1) (exprId + 1)
      nodeName = head (words currentLineValue)
   in ( [Node exprId (Host nodeName children [])],
        exprId',
        indentedLines'
      )

viewScanners = [viewScanner]

staticTextParserWrapper :: String -> ExprId -> ([Node View], ExprId)
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

elseScanner :: Scanner View
elseScanner ((line, currentIndentation, '#' : currentLineValue) : restIndentedLines) indentationLevel exprId
  | currentIndentation == indentationLevel && head currentLineValueWords == "else" =
      parseLines viewScanners restIndentedLines (currentIndentation + 1) exprId
  where
    currentLineValueWords = words currentLineValue
elseScanner indentedLines _ exprId = ([], exprId, indentedLines)
