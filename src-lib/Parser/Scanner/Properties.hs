module Parser.Scanner.Properties (propertiesScanner) where

import Parser.Util (parseLines)
import Types

propertiersScanner :: Scanner Properties
propertiersScanner [] indentationLevel exprId = ([], exprId, [])
propertiersScanner (indentedLines@(line, currentIndentation, currentLineValue) : restIndentedLines) indentationLevel exprId =
  let propertyWord = words currentLineValue
   in ( [Node exprId (Property (head propertyWord, ""))],
        exprId + 1,
        restIndentedLines
      )

propertiesScanner = [propertiersScanner]
