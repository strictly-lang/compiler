module Tokenizer.Main (tokenize) where

import Types

tokenize :: String -> [[Token]]
tokenize input =
  let (firstLine, succeedingLines) = tokenize' (0, 0) input
   in merge (Token (0, 0) (Indentation 0)) firstLine : succeedingLines

tokenize' :: Position -> String -> ([Token], [[Token]])
tokenize' _ [] = ([], [])
tokenize' position@(line, column) (' ' : restCharacters) =
  let (nextTokens, nextLines) = tokenize' (line, column + 1) restCharacters
   in (nextTokens, nextLines)
tokenize' position@(line, column) ('\n' : restCharacters)
  | null nextTokens = ([], nextLines)
  | otherwise  = ([], merge (Token (line, 0) (Indentation 0)) nextTokens : nextLines)
  where (nextTokens, nextLines) = tokenize' (line + 1, 0) restCharacters
tokenize' position@(line, column) ('\t' : restCharacters) =
  let (nextTokens, nextLines) = tokenize' (line, column + 1) restCharacters
   in (merge (Token position (Indentation 1)) nextTokens, nextLines)
tokenize' position@(line, column) ('#' : restCharacters) =
  let (nextToken, nextLines) = tokenize' (line, column + 1) restCharacters
   in (Token position Hash : nextToken, nextLines)
tokenize' position@(line, column) ('"' : restCharacters) =
  let (nextToken, nextLines) = tokenize' (line, column + 1) restCharacters
   in (Token position Quote : nextToken, nextLines)
tokenize' position@(line, column) ('$' : restCharacters) =
  let (nextToken, nextLines) = tokenize' (line, column + 1) restCharacters
   in (Token position Dollar : nextToken, nextLines)
tokenize' position@(line, column) ('{' : restCharacters) =
  let (nextToken, nextLines) = tokenize' (line, column + 1) restCharacters
   in (Token position LBrace : nextToken, nextLines)
tokenize' position@(line, column) ('}' : restCharacters) =
  let (nextToken, nextLines) = tokenize' (line, column + 1) restCharacters
   in (Token position RBrace : nextToken, nextLines)
tokenize' position@(line, column) ('_' : restCharacters) =
  let (nextToken, nextLines) = tokenize' (line, column + 1) restCharacters
   in (Token position Underscore : nextToken, nextLines)
tokenize' position@(line, column) ('<' : '-' : restCharacters) =
  let (nextToken, nextLines) = tokenize' (line, column + 1) restCharacters
   in (Token position Feed : nextToken, nextLines)
tokenize' position@(line, column) ('=' : '=' : restCharacters) =
  let (nextToken, nextLines) = tokenize' (line, column + 1) restCharacters
   in (Token position (LogicOperator "==") : nextToken, nextLines)
tokenize' position@(line, column) (currentCharacter : restCharacters) =
  let (nextToken, nextLines) = tokenize' (line, column + 1) restCharacters
   in (merge (Token position (Identity [currentCharacter])) nextToken, nextLines)

merge :: Token -> [Token] -> [Token]
merge (Token position (Indentation amount)) ((Token _position (Indentation nextAmount)) : ts) = Token position (Indentation (amount + nextAmount)) : ts
merge t@(Token position@(_, column) (Identity c)) ts@((Token (_, nextColumn) (Identity cs)) : ts')
  | column + 1 == nextColumn = Token position (Identity (c ++ cs)) : ts'
  | otherwise = t:ts++ts'

merge t ts = t : ts