module Compiler.Types.Root where

import Compiler.Types (AppState (AppState), AppStateMonad, Code (..))
import Compiler.Types.RootAssignment (rootAssignment)
import Compiler.Types.RootDeclaration (algebraicDataTypeConstructor)
import Control.Monad.State.Lazy (runState)
import Data.List (intersperse)
import Types

compileRoot :: String -> [Root] -> String
compileRoot componentName roots =
  let code = compileRoot' roots
      (result, _) = runState code (AppState componentName 0)
   in codeToString 0 True result

compileRoot' :: [Root] -> AppStateMonad [Code]
compileRoot' [] = do return []
compileRoot' (RootDataDeclaration _ dataDeclarations : restRoot) = do
  result <- algebraicDataTypeConstructor dataDeclarations
  next <- compileRoot' restRoot

  return (result ++ next)
compileRoot' (RootAssignment name expression : restRoot) = do
  result <- rootAssignment name expression
  next <- compileRoot' restRoot
  return (result ++ next)

codeToString :: Int -> Bool -> [Code] -> String
codeToString indentationLevel first [] = ""
codeToString indentationLevel first (Ind nestedCode : restCode) =
  codeToString (indentationLevel + 1) True nestedCode ++ "\n"
    ++ codeToString indentationLevel True restCode
codeToString indentationLevel first (Ln code : restCode)
  | first = '\n' : replicate indentationLevel '\t' ++ code'
  | otherwise = code'
  where
    code' = code ++ codeToString indentationLevel False restCode
codeToString indentationLevel first (Br : restCode) = '\n' : codeToString indentationLevel True restCode