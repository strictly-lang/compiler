module Parser.Scanner.View where

import Parser.Util (parseLines)
import Types

viewScanner :: Scanner View
viewScanner [] indentationLevel exprId = ([], exprId, [])
viewScanner (((Token _ Quote) : restLineTokens) : restIndentedLines) indentationLevel exprId =
  let (textNodes, exprId') = staticTextParserWrapper restLineTokens exprId
   in (textNodes, exprId', restIndentedLines)
viewScanner (((Token _ Hash) : (Token _ (Identity currentLineNameSpace)) : restLineTokens) : restIndentedLines) indentationLevel exprId
  | currentLineNameSpace == "if" =
    let (positiveChildren, exprId', indentedLines') = parseLines viewScanners restIndentedLines (indentationLevel + 1) (exprId + 1)
        (negativeChildren, exprId'', indentedLines'') = elseScanner indentedLines' indentationLevel exprId'
     in ( [Node exprId (Condition (Expr (conditionParser restLineTokens)) positiveChildren negativeChildren)],
          exprId'',
          indentedLines''
        )
  | currentLineNameSpace == "each" =
    let attributes = attributeParser restLineTokens
        (eachChild, exprId', indentedLines') = parseLines viewScanners restIndentedLines (indentationLevel + 1) (exprId + 1)
        (negativeChildren, exprId'', indentedLines'') = elseScanner indentedLines' indentationLevel exprId'
     in ( [Node exprId (Each attributes eachChild negativeChildren)],
          exprId'',
          indentedLines''
        )
viewScanner (((Token _ (Identity currentLineNameSpace)) : restLineTokens) : restIndentedLines) indentationLevel exprId =
  let (children, exprId', indentedLines') = parseLines viewScanners restIndentedLines (indentationLevel + 1) (exprId + 1)
      nodeName = currentLineNameSpace
   in ( [Node exprId (Host nodeName children [])],
        exprId',
        indentedLines'
      )

viewScanners = [viewScanner]

conditionParser :: [Token] -> String
conditionParser = concatMap show

attributeParser :: [Token] -> [Attribute]
attributeParser [] = []
attributeParser ts =
  let (l, ts') = leftExprParser ts
      (o, ts'') = operatorParser ts'
      (e, ts''') = exprParser ts''
      as = attributeParser ts'''
   in Attribute (l, o, e) : as

leftExprParser :: [Token] -> (LeftExpr, [Token])
leftExprParser (Token _ LParen : ts) =
  let (ls, ts') = leftExprParser' ts
   in (LeftTuple ls, ts')
leftExprParser (Token _ (Identity value) : ts) = (LeftVariable value, ts)

leftExprParser' :: [Token] -> ([LeftExpr], [Token])
leftExprParser' ((Token _ Comma) : ts) =
  let (l, ts') = leftExprParser ts
      (ls, ts'') = leftExprParser' ts'
   in (l : ls, ts'')
leftExprParser' ((Token _ RParen) : ts) = ([], ts)
leftExprParser' ts = 
  let (l, ts' ) = leftExprParser ts
      (ls, ts'') = leftExprParser' ts'
   in (l:ls, ts'')

operatorParser :: [Token] -> (Operator, [Token])
operatorParser (Token _ Feed : ts) = (FeedOperator, ts)

exprParser :: [Token] -> (Expr, [Token])
exprParser (Token _ (Identity value) : ts) = (Expr value, ts)

staticTextParserWrapper :: [Token] -> ExprId -> ([Node View], ExprId)
-- Do a syntax-error here, you cant end without a closing "
staticTextParserWrapper [] exprId = ([], exprId)
staticTextParserWrapper [Token _ Quote] exprId = ([], exprId)
staticTextParserWrapper ((Token _ Dollar) : (Token _ LBrace) : rest) exprId =
  let (dynamicText, rest') = dynamicTextParser rest
      (expr', exprId') = staticTextParserWrapper rest' (exprId + 1)
   in (Node exprId (DynamicText dynamicText) : expr', exprId')
staticTextParserWrapper cs exprId =
  let (staticText, rest) = staticTextParser cs
      (expr', exprId') = staticTextParserWrapper rest (exprId + 1)
   in (Node exprId (StaticText staticText) : expr', exprId')

data Text = Static String | Dynamic String

dynamicTextParser :: [Token] -> (String, [Token])
-- Do a syntax-error here, you cant end without a closing }
dynamicTextParser ((Token _ RBrace) : rest) = ("", rest)
dynamicTextParser (Token _ (Identity c) : cs) =
  let (cs', rest) = dynamicTextParser cs
   in (c ++ cs', rest)

staticTextParser :: [Token] -> (String, [Token])
-- Do a syntax-error here, you cant end without a closing }
staticTextParser [Token _ Quote] = ("", [])
staticTextParser all@((Token _ Dollar) : (Token _ LBrace) : rest) = ("", all)
staticTextParser (c : cs) =
  let (cs', rest) = staticTextParser cs
   in (show c ++ cs', rest)

elseScanner :: Scanner View
elseScanner (((Token currentPosition (Indentation currentIndentation)) : (Token _ Hash) : (Token _ (Identity currentLineNameSpace)) : restLineTokens) : restIndentedLines) indentationLevel exprId
  | currentIndentation == indentationLevel && currentLineNameSpace == "else" =
    parseLines viewScanners restIndentedLines (currentIndentation + 1) exprId
elseScanner indentedLines _ exprId = ([], exprId, indentedLines)
