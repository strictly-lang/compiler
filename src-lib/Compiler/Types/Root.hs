module Compiler.Types.Root where

import Compiler.Types (Code (..))
import Compiler.Types.RootDeclaration (algebraicDataTypeConstructor)
import Compiler.Util (pathToComponent)
import Data.List (intersperse)
import Types

compileRoot :: String -> [Root] -> String
compileRoot pathToComponent roots =
  codeToString 0 True (intersperse Br (compileRoot' pathToComponent roots))

compileRoot' :: String -> [Root] -> [Code]
compileRoot' pathToComponent [] = []
compileRoot' pathToComponent (RootDataDeclaration _ dataDeclarations : restRoot) =
  algebraicDataTypeConstructor dataDeclarations
    ++ compileRoot' pathToComponent restRoot
compileRoot' pathToComponent roots = []

codeToString :: Int -> Bool -> [Code] -> String
codeToString indentationLevel first [] = ""
codeToString indentationLevel first (Ind nestedCode : restCode) =
  '\n' :
  codeToString (indentationLevel + 1) True nestedCode
    ++ codeToString indentationLevel True restCode
codeToString indentationLevel first (Ln code : restCode)
  | first = replicate indentationLevel '\t' ++ rest
  | otherwise = rest
  where
    rest = code ++ codeToString indentationLevel False restCode
codeToString indentationLevel first (Br : restCode) = '\n' : codeToString indentationLevel True restCode