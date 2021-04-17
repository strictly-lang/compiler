module Parser.Scanner.Root (rootScanners) where

import Parser.Scanner.View (viewScanners)
import Parser.Util (parseLines)
import Types

rootScanner :: Scanner Root
rootScanner ([Token _ (Identity "view")] : restIndentedLines) currentIndentation exprId =
  let (children, exprId', restIndentedChildLines) = parseLines viewScanners restIndentedLines (currentIndentation + 1) (exprId + 1)
   in ( [Node exprId (View children)],
        exprId',
        restIndentedChildLines
      )
rootScanner [] _ exprId = ([], exprId, [])

rootScanners = [rootScanner]
