module Parser.Main where

parse content = map parseLine (lines content)


parseLine x = let (indent, value ) = getIndentation x;
            in show indent ++ ": " ++ value;

getIndentation :: String -> (Int, [Char])
getIndentation ('\t':xs) =
    let (indent, value) = getIndentation xs;
    in (indent + 1 , value)
getIndentation otherwise = (0, otherwise);