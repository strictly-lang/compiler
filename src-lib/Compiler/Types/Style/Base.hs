module Compiler.Types.Style.Base (compileStyle) where

import Compiler.Types
import Compiler.Util (getGetFreshExprId)
import Data.Char (toLower, toUpper)
import Data.List (intercalate)
import Types

compileStyle :: [StyleContent] -> ViewContent
compileStyle styleContents = Host (HostElement ("style", [], compileStyle' styleContents)) Nothing

compileStyle' :: [StyleContent] -> [ViewContent]
compileStyle' [] = []
compileStyle' (StyleContent (selector, rules) : nextStyleContents) = MixedText [StaticText (selector ++ "{\\n" ++ intercalate ";\\n" (compileStyleRule rules) ++ ";\\n}")] : compileStyle' nextStyleContents

compileStyleRule :: [Option RightHandSideValue] -> [String]
compileStyleRule [] = []
compileStyleRule ((rule, rightHandSideValue) : nextRules) = (rule ++ ": " ++ rightHandSideValueToCss rightHandSideValue ++ "") : compileStyleRule nextRules

cssUnits =
  [ -- Absolute Units
    "cm",
    "mm",
    "in",
    "px",
    "pt",
    "pc",
    -- Relative Units
    "em",
    "ex",
    "ch",
    "rem",
    "vw",
    "vh",
    "vmin",
    "vmax",
    "%"
  ]

uncapitalize :: String -> String
uncapitalize (firstChar : restString) = toLower firstChar : restString

rightHandSideValueToCss :: RightHandSideValue -> String
rightHandSideValueToCss (RightHandSideType name values)
  | isUnit && length values == 1 = rightHandSideValueToCss (head values) ++ loweredName
  | not isUnit && null values = loweredName
  | not isUnit = loweredName ++ "(" ++ intercalate "," (map rightHandSideValueToCss values) ++ ")"
  where
    loweredName = uncapitalize name
    isUnit = loweredName `elem` cssUnits
rightHandSideValueToCss (Number number) = show number
rightHandSideValueToCss (MixedTextValue [StaticText text]) = "\\\"" ++ text ++ "\\\""
rightHandSideValueToCss (RightHandSideList rightHandSdeValues []) = unwords (map rightHandSideValueToCss rightHandSdeValues)
